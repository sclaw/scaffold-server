function getHome() {
    if (window.getComputedStyle(document.getElementById('greetings'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "100px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('createaccount'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('about'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('news'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('legal'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('partnership'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    }
}
function getReg() {
    if (window.getComputedStyle(document.getElementById('greetings'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('createaccount'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "100px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('about'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('news'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('legal'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('partnership'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    }
}
function getAbout() {
    if (window.getComputedStyle(document.getElementById('greetings'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('createaccount'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('about'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "100px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('news'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('legal'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('partnership'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    }
}
function getNews() {
    if (window.getComputedStyle(document.getElementById('greetings'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('createaccount'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('about'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('news'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "100px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('legal'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('partnership'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    }
}
function getLegal() {
    if (window.getComputedStyle(document.getElementById('greetings'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('createaccount'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('about'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('news'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('legal'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "100px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('partnership'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    }
}
function getPart() {
    if (window.getComputedStyle(document.getElementById('greetings'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('createaccount'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('about'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('news'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('legal'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "100px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "-1200px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('partnership'),null).getPropertyValue("left") === "100px") {
        document.getElementById('greetings').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('createaccount').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('about').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('news').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('legal').animate([{left: "-1200px"}, {left: "-1200px"}], {duration: 600, fill: "forwards", easing: "ease"});
        document.getElementById('partnership').animate([{left: "100px"}, {left: "100px"}], {duration: 600, fill: "forwards", easing: "ease"});
    }
}
function getLogin() {
    if (window.getComputedStyle(document.getElementById('loginblock'),null).getPropertyValue("right") === "-200px") {
        document.getElementById('loginblock').animate([{right: "-200px"}, {right: "40px"}], {duration: 600, fill: "forwards", easing: "ease"});
    } else if (window.getComputedStyle(document.getElementById('loginblock'),null).getPropertyValue("right") === "40px") {
        document.getElementById('loginblock').animate([{right: "40px"}, {right: "-200px"}], {duration: 600, fill: "forwards", easing: "ease"});
    }
}