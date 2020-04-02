// https://stackoverflow.com/questions/895171/prevent-users-from-submitting-a-form-by-hitting-enter
$(document).ready(function() {
  $(window).keydown(function(event){
    if(event.keyCode == 13 && !$('textarea').is(':focus')) {
      event.preventDefault();
      return false;
    }
  });
});

(isLoggedIn() ? $(".logged_in") : $(".logged_out")).removeClass("template");

var userId = parseInt(getCookie("_userId"));
var token = getCookie("_token");
var user = null;

if (isLoggedIn()) {
    getUser(parseInt(userId), (usr) => {
        console.log(usr);
        $(".my_nickname").text(usr.searchNickname); 
        $(".my_nickname_input").val(usr.searchNickname); 
        $(".my_dodocode").text(usr.searchDodoCode);
        $(".my_dodocode_input").val(usr.searchDodoCode);
        $(".my_switchfc").text(usr.searchSwitchFc);
        $(".my_switchfc_input").val(usr.searchSwitchFc);
        $(".my_favvillager").text(usr.searchFavVillager);
        $(".my_favvillager_input").val(usr.searchFavVillager);
        $(".my_favthing").text(usr.searchFavThing);
        $(".my_favthing_input").val(usr.searchFavThing);
        $(".my_bio").text(usr.searchBio);
        $(".my_bio_input").val(usr.searchBio);

        $("input[name=islandOpenSelector][value=" + usr.searchIslandOpen + "]").prop('checked', true).click();

    
        user = usr;
    
        getPriceByUser(userId, (p) => {
            if (p) {
                $(".my_price_input").val(p.price);

                if (isPriceStale(p)) {
                    $(".nostale.nostale_price").removeClass("nostale");
                }
            }
        });
        
        getItemByUser(userId, (i) => {
            if (i) {
                $(".my_item_input").val(i.hotItem);

                if (isItemStale(i)) {
                    $(".nostale.nostale_item").removeClass("nostale");
                }
            }
        });

    });
}
