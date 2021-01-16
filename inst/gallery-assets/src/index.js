////////////////////////////////////////////////////////////////////////////////
// FILE: index.js
// AUTHOR: David Ruvolo
// CREATED: 2020-06-28
// MODIFIED: 2021-01-16
// PURPOSE: main entry
// DEPENDENCIES: NA
// STATUS: working
// COMMENTS: NA
////////////////////////////////////////////////////////////////////////////////

// import scss
import "./scss/index.scss"

////////////////////////////////////////

// define binding for icon button
const iconButton = new Shiny.InputBinding();
$.extend(iconButton, {
    find: function (scope) {
        return $(scope).find(".icon-button");
    },
    initialize: function(el) {
        return $(el).data("value");
    },
    getValue: function (el) {
        return $(el).data("value");
    },
    subscribe: function (el) {
        $(el).on("click", function(e) {
            var d = $(el).data("value");
            navigator.clipboard.writeText(d).then(function() {
                console.log("copied: ", d);
                $("#status-success").addClass("show");
                $("#status-success").find("p").text(`Copied '${$(el).data("icon")}'`);
                setTimeout(function() {
                    $("#status-success").removeClass("show");
                }, 3500)
            }, function() {
                $("#status-failed").addClass("show");
                setTimeout(function() {
                    $("#status-failed").removeClass("show");
                }, 3500)
            })
        });
    },
    unsubscribe: function (el) {
        $(el).off(".iconButton");
    }
});

// bind input
Shiny.inputBindings.register(iconButton);

////////////////////////////////////////


// new binding for select input dropdowns
var selectInputToggle = new Shiny.InputBinding();
$.extend(selectInputToggle, {
    find: function (scope) {
        return $(scope).find(".select-input-group");
    },
    initialize: function (el) {

        // get value of first dropdown element
        var elem = $(el).find(".select-input-option-button").first()
        var val = elem.data("value");
        var txt = elem.text();

        // set
        elem.addClass("selected");
        $(el).find(".select-input-selected").text(txt);
        $(el).data("value", val);
    },
    getValue: function (el) {
        var n = $(el).data("value");
        return n;
    },
    subscribe: function (el, callback) {

        // call elements
        var selectText = $(el).find(".select-input-selected");
        var selectMenu = $(el);

        // event to toggle dropdown
        $(el).on("click", "button.select-input-parent", function (e) {
            ;
            selectMenu.toggleClass("hidden");
            if (!selectMenu.hasClass("hidden")) {
                selectText.text("----");
            }
        });

        // event to update value
        $(el).on("click", "button.select-input-option-button", function (e) {

            // remove classes
            $(el).find(".select-input-option-button").removeClass("selected");

            // select elements
            var val = $(e.target).data("value");
            var txt = $(e.target).text();

            // update elements
            $(el).data("value", val);
            $(e.target).addClass("selected");
            selectText.text(txt);
            selectMenu.toggleClass("hidden");

            // run getValue
            callback();
        })

        // blur event for parent element
        $(el).on("focus", "button.select-input-parent", function (e) {
            selectMenu.toggleClass("hidden");
        });

        // focus and blur events for option buttons
        var btns = $(el).find("button.select-input-option-button");
        $(btns).on("focus", function (e) {

            // ensure menu is open
            selectMenu.removeClass("hidden");
            var lastIndex = btns.length - 1
            var lastBtn = btns[lastIndex];

            // close menu on last blur
            if (e.target.nextSibling) {
                $(lastBtn).on("blur", function (e) {
                    selectMenu.addClass("hidden");
                });
            }
        });
    },
    unsubscribe: function(el) {
        $(el).off(".selectInputToggle");
    }
});

Shiny.inputBindings.register(selectInputToggle);