// https://stackoverflow.com/questions/36412407/shiny-add-link-to-another-tabpanel-in-another-tabpanel

var fakeClick = function (tabName) {
    var dropdownList = document.getElementsByTagName("a");
    for (var i = 0; i < dropdownList.length; i++) {
        var link = dropdownList[i];
        if (link.getAttribute("data-value") == tabName) {
            link.click();
            // https://github.com/nuxt/nuxt/issues/25816
            window.scrollTo({
                top:0, 
                left: 0, 
                behavior: "instant"
            });
        };
    };
};


