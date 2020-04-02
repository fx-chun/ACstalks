var urlParams = new URLSearchParams(window.location.search);

function populateProfile( userId ) {
    queryJSON("POST", "/api/user/search?", [ userId ], (users) => {
        user = users[0];

        var nick = user.searchNickname;
        var bio = user.searchBio;
        var favvillager = user.searchFavVillager;
        var favthing = user.searchFavThing;
        var switchfc = user.searchSwitchFc;

        $(".profile_nickname").text(nick);
        $(".profile_bio").text(bio);
        $(".profile_favvillager").text(favvillager);
        $(".profile_favthing").text(favthing);
        $(".profile_switchfc").text(switchfc);

        var islandText;

        switch (user.searchIslandOpen) {
            case "IslandOpen":
                islandText = ["open to the public.", "islandopen_open"];
                break;
            case "IslandFriends":
                islandText = ["open to friends only.", "islandopen_friends"];
                break;
            case "IslandSeeBio":
                islandText = ["open. See my bio!", "islandopen_seebio"];
                break;
            case "IslandClosed":
                islandText = ["closed.", "islandopen_closed"];
                break;
        }

        $(".profile_islandopen").text(islandText[0]).addClass(islandText[1]);
    });

    queryJSON("GET", "/api/market/user?id=" + userId, null, (price) => {
        if (price) {
            var time = moment.utc(price.priceTime).utcOffset(price.priceTimezone * -1);
            var timeFormat = time.format("LLLL");
            
            if (isPriceStale(price)) {
                timeFormat = timeFormat + " (price is now stale)";
                $("#price_container").addClass("stale");
            }

            $(".profile_price").text(price.price);
            $(".profile_price_time").text(timeFormat);
        }
    });

    queryJSON("GET", "/api/hotitem/user?id=" + userId, null, (hotItem) => {
        if (hotItem) {
            var time = moment.utc(hotItem.hotItemTime).utcOffset(hotItem.hotItemTimezone * -1);
            var timeFormat = time.format("LLLL");
 
            if (isItemStale(hotItem)) {
                timeFormat = timeFormat + " (item is now stale)";
                $("#hotitem_container").addClass("stale");
            }

            $(".profile_hotitem").text(hotItem.hotItem);
            $(".profile_hotitem_time").text(timeFormat);
        }
    });
}

if (!isLoggedIn() && !(urlParams.has("id"))) {
    window.location.href = "/"; 
}

if (urlParams.has("id")) {
    populateProfile(parseInt(urlParams.get("id")));
} else {
    populateProfile(parseInt(getCookie("_userId")));
    window.history.pushState("", "", '/profile.html?id=' + getCookie("_userId"));
}
