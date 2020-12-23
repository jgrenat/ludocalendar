import "./styles/reset.css";
import "./styles/styles.css";
// @ts-ignore
const { Elm } = require("./src/Main.elm");
const pagesInit = require("elm-pages");

pagesInit({
  mainElmModule: Elm.Main
}).then(elmModule => {
  elmModule.ports.saveToLocalStorage.subscribe(({day, model}) => {
    localStorage.setItem("day" + day.toString(), JSON.stringify(model));
  });
  elmModule.ports.initComments.subscribe(() => {
    setTimeout(() => {
      window.commentBox('5701926784073728-proj');
    });
  });
  elmModule.ports.stateFromLocalStorage.send({
    day1: JSON.parse(localStorage.getItem("day1")),
    day2: JSON.parse(localStorage.getItem("day2")),
    day3: JSON.parse(localStorage.getItem("day3")),
    day4: JSON.parse(localStorage.getItem("day4")),
    day5: JSON.parse(localStorage.getItem("day5")),
    day6: JSON.parse(localStorage.getItem("day6")),
    day7: JSON.parse(localStorage.getItem("day7")),
    day8: JSON.parse(localStorage.getItem("day8")),
    day9: JSON.parse(localStorage.getItem("day9")),
    day10: JSON.parse(localStorage.getItem("day10")),
    day11: JSON.parse(localStorage.getItem("day11")),
    day12: JSON.parse(localStorage.getItem("day12")),
    day13: JSON.parse(localStorage.getItem("day13")),
    day14: JSON.parse(localStorage.getItem("day14")),
    day15: JSON.parse(localStorage.getItem("day15")),
    day16: JSON.parse(localStorage.getItem("day16")),
    day17: JSON.parse(localStorage.getItem("day17")),
    day18: JSON.parse(localStorage.getItem("day18")),
    day19: JSON.parse(localStorage.getItem("day19")),
    day20: JSON.parse(localStorage.getItem("day20")),
    day21: JSON.parse(localStorage.getItem("day21")),
    day22: JSON.parse(localStorage.getItem("day22")),
    day23: JSON.parse(localStorage.getItem("day23")),
    day24: JSON.parse(localStorage.getItem("day24")),
  });
});


// Add Plausible
const scriptTag = document.createElement('script');
scriptTag.type = 'text/javascript';
scriptTag.async = true;
scriptTag.src = 'https://plausible.io/js/plausible.js';
scriptTag.defer = true;
scriptTag['data-domain'] = 'ludocalendar.com';
window.document.body.appendChild(scriptTag);


// Add Plausible
const commentBoxScriptTag = document.createElement('script');
commentBoxScriptTag.type = 'text/javascript';
commentBoxScriptTag.async = true;
commentBoxScriptTag.src = 'https://unpkg.com/commentbox.io/dist/commentBox.min.js';
commentBoxScriptTag.defer = true;
commentBoxScriptTag.onload = () => window.commentBox('5701926784073728-proj');
window.document.body.appendChild(commentBoxScriptTag);




