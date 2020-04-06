var priceListSortDesc = true;
var hotItemSortAlpha = false;

var priceList = createListUi( "Price List ", () => {
    priceListSortDesc = !priceListSortDesc;
    updatePriceUi(priceList, priceListSortDesc);
});

var hotItemList = createListUi( "Item List", () => {
    hotItemSortAlpha = !hotItemSortAlpha;
    if ($('#item_search_query').val())
    {
        searchItem($('#item_search_query').val(), hotItemSortAlpha); 
    } else {
        updateHotItemUi(hotItemList, hotItemSortAlpha);
    }
});

priceList.appendTo("#main_pricelist");
hotItemList.appendTo("#main_hotlist");

function timetravelerUpdateUi()
{
    if ($('#timetraveler_check').is(":checked")) { 
        $(".timetraveler").removeClass("template"); 
        $(".not_timetraveler").addClass("template"); 
    } else { 
        $(".timetraveler").addClass("template"); 
        $(".not_timetraveler").removeClass("template"); 
    }
}

function updateTimeLoop()
{
    var time = moment();
    var timeString = time.format("dddd, MMMM Do YYYY, h:mm:ss a"); 

    $(".currentTime").val(timeString);
    $(".localMarketStatus").css("background-color", "#d1d1d1");

    var hour = time.hour();

    console.log(hour);

    if (time.day() == 0 && (hour >= 6) && (hour <= 12)) {
        $("#indicator_buy").css("background-color", "#db5757");
    } else if (time.day() != 0 && (hour >= 8) && (hour <= 22)) {
        $("#indicator_sell").css("background-color", "#339933");
    } else {
        $("#indicator_closed").css("background-color", "#5778db");
    }

}

timetravelerUpdateUi();
updatePriceUi(priceList, priceListSortDesc);
updateHotItemUi(hotItemList, hotItemSortAlpha);
updateTimeLoop();
setInterval(updateTimeLoop, 1000);
