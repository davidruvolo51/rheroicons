////////////////////////////////////////////////////////////////////////////////
// FILE: index.js
// AUTHOR: David Ruvolo
// CREATED: 2020-06-28
// MODIFIED: 2022-10-16
// PURPOSE: Shiny extensions
// DEPENDENCIES: NA
// STATUS: working
// COMMENTS: NA
////////////////////////////////////////////////////////////////////////////////

import './scss/index.scss';

const iconButton = new Shiny.InputBinding();
const selectInputToggle = new Shiny.InputBinding();

$.extend(iconButton, {
  find: function (scope) {
    return $(scope).find(".icon-button");
  },
  initialize: function (el) {
    return $(el).data("value");
  },
  getValue: function (el) {
    return $(el).data("value");
  },
  subscribe: function (el) {
    $(el).on("click", function (e) {
      const d = $(el).data("value");
      navigator.clipboard.writeText(d).then(function () {
        console.log("copied: ", d);
        $("#status-success").addClass("show");
        $("#status-success").find("p").text(`Copied '${$(el).data("icon")}'`);
        setTimeout(function () {
          $("#status-success").removeClass("show");
        }, 3500)
      }, function () {
        $("#status-failed").addClass("show");
        setTimeout(function () {
          $("#status-failed").removeClass("show");
        }, 3500)
      })
    });
  },
  unsubscribe: function (el) {
    $(el).off(".iconButton");
  }
});

$.extend(selectInputToggle, {
  find: function (scope) {
    return $(scope).find(".select-input-group");
  },
  initialize: function (el) {
    const elem = $(el).find(".select-input-option-button").first()
    const val = elem.data("value");
    const txt = elem.text();

    elem.addClass("selected");
    $(el).find(".select-input-selected").text(txt);
    $(el).data("value", val);
  },
  getValue: function (el) {
    const n = $(el).data("value");
    return n;
  },
  subscribe: function (el, callback) {
    const selectText = $(el).find(".select-input-selected");
    const selectMenu = $(el);

    $(el).on("click", "button.select-input-parent", function (e) {
      selectMenu.toggleClass("hidden");
      if (!selectMenu.hasClass("hidden")) {
        selectText.text("----");
      }
    });

    $(el).on("click", "button.select-input-option-button", function (e) {
      $(el).find(".select-input-option-button").removeClass("selected");

      const val = $(e.target).data("value");
      const txt = $(e.target).text();

      $(el).data("value", val);
      $(e.target).addClass("selected");
      selectText.text(txt);
      selectMenu.toggleClass("hidden");

      callback();
    })

    $(el).on("focus", "button.select-input-parent", function (e) {
      selectMenu.toggleClass("hidden");
    });

    const btns = $(el).find("button.select-input-option-button");
    $(btns).on("focus", function (e) {
      selectMenu.removeClass("hidden");
      const lastIndex = btns.length - 1
      const lastBtn = btns[lastIndex];

      if (e.target.nextSibling) {
        $(lastBtn).on("blur", function (e) {
          selectMenu.addClass("hidden");
        });
      }
    });
  },
  unsubscribe: function (el) {
    $(el).off(".selectInputToggle");
  }
});

Shiny.inputBindings.register(iconButton);
Shiny.inputBindings.register(selectInputToggle);
