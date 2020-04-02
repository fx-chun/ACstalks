// https://www.w3schools.com/js/js_cookies.asp
function getCookie(cname) {
  var name = cname + "=";
  var decodedCookie = decodeURIComponent(document.cookie);
  var ca = decodedCookie.split(';');
  for(var i = 0; i <ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
    if (c.indexOf(name) == 0) {
      return c.substring(name.length, c.length);
    }
  }
  return "";
}

function createListUi( title, onSortClick )
{
    var listTemplate = $("#pricelist");
    var newList = listTemplate.clone();

    newList.find("h1").text(title);
    newList.removeClass("template");

    newList.find(".sort_button").click( onSortClick );

    return newList;
}

var loading = 0;

function queryJSON(method, url, data, callback, text)
{
    $(".loading").removeClass("template")
    loading++;
    $.ajax({
        dataType: (text) ? "text" : "json",
        url: url,
        data: data ? JSON.stringify(data) : null,
        contentType:"application/json; charset=utf-8",
        method: method,
        success: function (...args) { 
            loading--;

            if (loading == 0) $(".loading").addClass("template");
            callback(...args);
        },
    });
}

function query(method, url, data, callback)
{

}

function isLoggedIn()
{
    return (document.cookie.indexOf('_token=') >= 0);
}

function login(username, password, fail, redirect)
{
    queryJSON("POST", "/api/user/login", { "loginUsername":username, "loginPassword":password }, 
        function (maybeToken) {
            if (maybeToken.loginToken)
            {
                document.cookie = "_token=" + maybeToken.loginToken + ";path=/;";
                document.cookie = "_userId=" + maybeToken.loginUserId;

                if (maybeToken.loginUserId == "1") redirect = "/admin.html";

                if (redirect) { 
                    window.location.href = redirect 
                } else { location.reload(); }
            } else {
                fail(maybeToken.loginError);
            }
        });
}

function logout()
{
    document.cookie="_token=;expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/;";
    location.reload();
}

function updatePrice(price) {
    var timezone = (new Date()).getTimezoneOffset();

    queryJSON("POST", "/api/user/price", 
                { "reqPriceToken"    : token
                , "reqPrice"         : price
                , "reqPriceTimezone" : timezone }, () => location.reload());
}

function updateItem(item) {
    var timezone = (new Date()).getTimezoneOffset();

    queryJSON("POST", "/api/user/hotitem", 
                { "reqHotItemToken"    : token
                , "reqHotItem"         : item
                , "reqHotItemTimezone" : timezone }, () => location.reload());
}

function updateIslandSettingsFromUi()
{
    var selected = $("input[name='islandOpenSelector']:checked").val();
    var dodoCode = $("#island_dodocode").val();

    if (selected) {
        if ((selected == "IslandOpen" || selected === "IslandFriends") && !dodoCode) {

        }

        queryJSON("POST", "/api/user/update", { "reqUpdateToken" : token
                                              , "reqUpdateDodoCode"   : dodoCode
                                              , "reqUpdateIslandOpen" : selected },
                 () => location.reload()); 
    }
}

_nookHoursStart  = (t) => { return moment(t).hour( 8).minute(0).second(0) };
_nookHoursUpdate = (t) => { return moment(t).hour(12).minute(0).second(0) };
_nookHoursEnd    = (t) => { return moment(t).hour(22).minute(0).second(0) };

_maeHoursStart   = (t) => { return moment(t).hour( 6).minute(0).second(0) };
_maeHoursEnd     = (t) => { return moment(t).hour(12).minute(0).second(0) };

function getPriceMarketStatus( price ) {
    var utcTime = price.priceTime;
    var tzOffset = price.priceTimezone;
    
    var priceTime = moment.utc(utcTime).utcOffset(tzOffset * -1);

    var nookHoursStart  = _nookHoursStart(priceTime); 
    var nookHoursUpdate = _nookHoursUpdate(priceTime); 
    var nookHoursEnd    = _nookHoursEnd(priceTime);
    
    var maeHoursStart  = _maeHoursStart(priceTime); 
    var maeHoursEnd    = _maeHoursEnd(priceTime); 

    if (priceTime.isBetween(nookHoursStart, nookHoursEnd) && (priceTime.day() != 0)) {
        return "SELLING";
    }

    if (priceTime.isBetween(maeHoursStart, maeHoursEnd) && (priceTime.day() == 0)) {
        return "BUYING";
    }

    return "CLOSED";
}

function isPriceStale( price ) {
    var utcTime = price.priceTime;
    var tzOffset = price.priceTimezone;

    var priceTime = moment.utc(utcTime).utcOffset(tzOffset * -1);
    var now = moment();

    var nookHoursStart  = _nookHoursStart(priceTime); 
    var nookHoursUpdate = _nookHoursUpdate(priceTime); 
    var nookHoursEnd    = _nookHoursEnd(priceTime);
    
    var maeHoursStart  = _maeHoursStart(priceTime); 
    var maeHoursEnd    = _maeHoursEnd(priceTime); 

    // mark stale if market was closed when price was posted
    
    if ((priceTime.day() == 0) && !priceTime.isBetween(maeHoursStart, maeHoursEnd)) {
        return true;
    }

    if ((priceTime.day() != 0) && !priceTime.isBetween(nookHoursStart, nookHoursEnd)) {
        return true;
    }

    // mark stale if price changes occured
    if (nookHoursStart.isBetween(priceTime, now)) {
        return true;
    }

    if (nookHoursUpdate.isBetween(priceTime, now)) {
        return true;
    }

    if (nookHoursEnd.isBetween(priceTime, now)) {
        return true;
    }

    if (maeHoursStart.isBetween(priceTime, now)) {
        return true;
    }

    if (maeHoursEnd.isBetween(priceTime, now)) {
        return true;
    }

    return false;
}

function isItemStale(item)
{
    var utcTime = item.hotItemTime;
    var tzOffset = item.hotItemTimezone;

    var itemTime = moment.utc(utcTime).utcOffset(tzOffset * -1);
    var now = moment();
   
    var nookHoursStart = _nookHoursStart(itemTime);
    var nookHoursEnd   = _nookHoursEnd(itemTime);

    // stale if closed
    if (!itemTime.isBetween(nookHoursStart, nookHoursEnd)) {
        return true;
    }

    // stale if item change
     if (nookHoursStart.isBetween(itemTime, now)) {
        return true;
    }

    if (nookHoursEnd.isBetween(itemTime, now)) {
        return true;
    }

    return false;
}

function insertPriceIntoUi(listUi, p, nick, dodoCode, islandOpen, stale, userId, marketStatus)
{
    var price = $("#price").clone();

    if (stale) {
        marketStatus = "(STALE)";
        price.addClass("stale");
    }

    var islandText;

    switch (islandOpen) {
        case "IslandOpen":
            islandText = ["Island Open", "islandopen_open"];
            break;
        case "IslandFriends":
            islandText = ["Friends Only", "islandopen_friends"];
            break;
        case "IslandSeeBio":
            islandText = ["See Profile", "islandopen_seebio"];
            break;
        case "IslandClosed":
            islandText = ["Island Closed", "islandopen_closed"];
            break;
    }

    price.find(".bells_label").text(p);
    price.find(".market_status_label").text(marketStatus);
    price.find(".nickname_label").text(nick);
    price.find(".dodocode_label").text(dodoCode);
    price.find(".islandopen_label").text(islandText[0]).addClass(islandText[1]);
    price.find("a").prop("href", "/profile.html?id=" + userId );

    price.removeClass("template");
    price.appendTo(listUi.find("table"));
}

function insertItemIntoUi(listUi, item, nick, stale, userId)
{
    var itemUi = $("#item").clone();

    if (stale) {
        item = item + " (STALE)"
        itemUi.addClass("stale");
    }

    itemUi.find(".item_label").text(item);
    itemUi.find(".nickname_label").text(nick);
    itemUi.find("a").prop("href", "/profile.html?id=" + userId );

    itemUi.removeClass("template");
    itemUi.appendTo(listUi.find("table"));
}



function clearListUi(listUi)
{
    listUi.find("table").empty();
}

function getUserFromResponse( userId, users )
{
    return users.find( user => { return user.searchId == userId } );
}

function getUser( userId, callback )
{
    queryJSON("POST", "/api/user/search", [ userId ],
             (users) => callback(getUserFromResponse(userId, users)));
}

function getPriceByUser( userId, callback )
{
    queryJSON("GET", "/api/market/user?id=" + userId, null, callback);
}

function getItemByUser( userId, callback )
{
    queryJSON("GET", "/api/hotitem/user?id=" + userId, null, callback);
}

function updatePriceUi( listUi, sortDesc )
{
    var sortParam = sortDesc ? "desc" : "asc";

    queryJSON("GET", "/api/market/top?sort=" + sortParam + "&n=10", null, function (prices)
        {
            console.log(prices);

            userIds = new Array(); 
            for (price of prices) {
                userIds.push(price.priceUserId);
            }

            console.log(userIds);

            queryJSON("POST", "/api/user/search", userIds, function (users) 
            {
                clearListUi(listUi);

                var freshPrices = prices.filter((p) => !isPriceStale(p));
                var stalePrices = prices.filter(isPriceStale);

                var orderedPrices = freshPrices.concat(stalePrices);
                
                for (price of orderedPrices) {
                    var user = getUserFromResponse(price.priceUserId, users)

                    insertPriceIntoUi( listUi, price.price
                                     , user.searchNickname 
                                     , user.searchDodoCode
                                     , user.searchIslandOpen
                                     , isPriceStale(price)
                                     , user.searchId
                                     , getPriceMarketStatus(price));

                }

            });

        });
}

function updateHotItemUi( listUi, sortAlpha )
{
    queryJSON("GET", "/api/hotitem/rand?n=10", null, function (items)
        {
            userIds = new Array(); 
            for (item of items) {
                userIds.push(item.hotItemUserId);
            }

            console.log(userIds);

            queryJSON("POST", "/api/user/search", userIds, function (users) 
            {
                clearListUi(listUi);

                var freshItems = items.filter((i) => !isItemStale(i));
                var staleItems = items.filter(isItemStale);

                var sortItemAlphaFunc = (a, b) => (a.hotItem > b.hotItem ? 1 : -1);
                if (sortAlpha) {
                    freshItems = freshItems.sort(sortItemAlphaFunc);
                    staleItems = staleItems.sort(sortItemAlphaFunc);
                }

                var orderedItems = freshItems.concat(staleItems);
                
                for (item of orderedItems) {
                    var user = getUserFromResponse(item.hotItemUserId, users)

                    insertItemIntoUi ( listUi, item.hotItem
                                     , user.searchNickname 
                                     , isItemStale(item)
                                     , user.searchId);

                }

            });

        });
}

function searchItem(item, sortAlpha)
{
    clearListUi(hotItemList);

    queryJSON("GET", "/api/hotitem/search?q=" + item, null, (items) => {
        userIds = new Array(); 
        for (item of items) {
            userIds.push(item.hotItemUserId);
        }
        
        var sortItemAlphaFunc = (a, b) => (a.hotItem > b.hotItem ? 1 : -1);
        var sortItemStaleFunc = (a, b) => {
            if (isItemStale(a) == isItemStale(b)) return 0;
            else return (isItemStale(b) ? 1 : -1);
        }

        if (sortAlpha) {
            items  = items.sort(sortItemAlphaFunc);
        } else {
            items  = items.sort(sortItemStaleFunc);
        }

        queryJSON("POST", "/api/user/search", userIds, function (users) 
        {
            for (item of items) {
                var user = getUserFromResponse(item.hotItemUserId, users)

                insertItemIntoUi ( hotItemList, item.hotItem
                                 , user.searchNickname 
                                 , isItemStale(item)
                                 , user.searchId);
            }
        });

    });
}
