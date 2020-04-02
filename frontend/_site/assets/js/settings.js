if (!isLoggedIn()) window.location.href="/";

function saveSettings()
{
    if ($("#settings_password").val() && !($("#settings_password_confirm").val())) {
        alert("Please confirm your password.");
        return;
    }

    if ($("#settings_password").val() != $("#settings_password_confirm").val()) {
        alert("New password does not match confirmation.");
        return;
    }

    var query = {
        "reqUpdateToken" : token,
        "reqUpdateNickname" : $("#settings_nickname").val(),
        "reqUpdatePassword" : ($("#settings_password").val()) ? $("#settings_password").val() : null,
        "reqUpdateSwitchFc" : $("#settings_switchfc").val(),
        "reqUpdateBio"      : $("#settings_bio").val(),
        "reqUpdateFavVillager" : $("#ac-select").val(),
        "reqUpdateFavThing" : $("#ac-select-2").val()
    };

    queryJSON("POST", "/api/user/update", query, (a) =>
        {
            if (a) {
                alert("Profile updated.");
                location.reload();
            }
        });
}
