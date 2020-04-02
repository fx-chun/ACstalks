function submitRegistration()
{
    if ($("#agreement").prop("checked")) {
        queryJSON("POST", "/api/user/register",
                 {
                     "regUsername" : $("#register_username").val(),
                     "regNickname" : $("#register_nickname").val(),
                     "regPassword" : $("#register_password").val(),
                     "regSwitchFc" : "",
                     "regCaptcha"  : grecaptcha.getResponse()
                 }, (result) =>
                 {
                    if (result.regToken) {
                        login( $("#register_username").val(), $("#register_password").val(), () => 0, "/settings.html");
                    } else {
                        $("#register_error").text(result.regError);
                    }
                 });
    } else {
        $("#register_error").text("Please agree to the terms outlined above.");
    }
}
