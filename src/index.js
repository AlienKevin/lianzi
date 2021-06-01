import './main.css';
import './elm-pep';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import TraceSkeleton from 'skeleton-tracing-wasm';
import Canvg from 'canvg';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

var charList;

fetch("常用國字表.txt").then(function(response) {
  return response.text().then(function(txt) {
    charList = txt.split("\n");
  });
});

const corsProxy = "https://morning-taiga-42342.herokuapp.com/";
let csldCharacterUrl = "";
let staticCsldCharacterUrl = "";

app.ports.getNextCharInListPort.subscribe(function (nextCharId) {
  app.ports.setCurrCharInListPort.send(charList[nextCharId]);
});

app.ports.checkCsldCharacterPort.subscribe(function () {
  // ref-character
  var refPromise = new Promise(function(resolve) {
    var refCanvas = document.createElement("canvas");
    const characterSize = document.getElementById("ref-character").clientWidth;
    refCanvas.width = characterSize;
    refCanvas.height = characterSize;
    var refCtx = refCanvas.getContext("2d");
    var refImage = new Image();
    refImage.src = staticCsldCharacterUrl;
    refImage.onload = function () {
      refCtx.drawImage(refImage, 0, 0, refCanvas.width, refCanvas.height);
      const refImgData = refCtx.getImageData(0, 0, refCanvas.width, refCanvas.height);
      preprocessForTracer(refImgData);
      // trace the b-and-w image
      TraceSkeleton.load().then(tracer => {
        const { polylines } = tracer.fromImageData(refImgData);
        resolve(polylines);
        // console.log("ref-character:");
        // console.log(polylines)
      });
    };
  });

  // user-character
  var userPromise = new Promise(function(resolve, reject) {
    var userSvg = document.getElementById("user-character");
    const characterSize = document.getElementById("ref-character").clientWidth;
    if (userSvg !== null) {
      var userSvgString = new XMLSerializer().serializeToString(userSvg);
      const userCanvas = document.createElement('canvas');
      userCanvas.width = characterSize;
      userCanvas.height = characterSize;
      const userCtx = userCanvas.getContext('2d');
      var v = Canvg.fromString(userCtx, userSvgString);
      v.render().then(function() {
        var userImgData = userCtx.getImageData(0, 0, userCanvas.width, userCanvas.height);
        preprocessForTracer(userImgData);
        console.log("userImgData: ", userImgData);
        // trace the b-and-w image
        TraceSkeleton.load().then(tracer => {
          const { polylines, rects } = tracer.fromImageData(userImgData);
          resolve(polylines);
          // console.log("user-character:");
          // console.log(tracer.visualize({polylines, rects}));
          // console.log(polylines);
        });
      });
    } else {
      reject();
    }
  });

  // check
  Promise.all([refPromise, userPromise]).then(function([refPolylines, userPolylines]) {
    var userSvg = document.getElementById("user-character");
    var svgns = "http://www.w3.org/2000/svg";
    var isAllGood = true;
    userPolylines.forEach(function(userLine) {
      userLine.forEach(function(userPoint) {
        var {neighbor, distance} = findNearestNeighbor(refPolylines, userPoint);
        if (distance >= 20) {
          isAllGood = false;
          var circle = document.createElementNS(svgns, 'circle');
          circle.setAttributeNS(null, 'cx', userPoint[0]);
          circle.setAttributeNS(null, 'cy', userPoint[1]);
          circle.setAttributeNS(null, 'r', 5);
          circle.setAttributeNS(null, 'style', 'fill: #33ccff;' );
          userSvg.appendChild(circle);
          setTimeout(function() {
            fadeOut(circle);
          }, 2000);
        }
      });
    });
    refPolylines.forEach(function(refLine) {
      refLine.forEach(function(refPoint) {
        var {neighbor, distance} = findNearestNeighbor(userPolylines, refPoint);
        if (distance >= 20) {
          isAllGood = false;
          var circle = document.createElementNS(svgns, 'circle');
          circle.setAttributeNS(null, 'cx', refPoint[0]);
          circle.setAttributeNS(null, 'cy', refPoint[1]);
          circle.setAttributeNS(null, 'r', 5);
          circle.setAttributeNS(null, 'style', 'fill: #33ccff;' );
          userSvg.appendChild(circle);
          setTimeout(function() {
            fadeOut(circle);
          }, 2000);
        }
      });
    });
    if (isAllGood) {
      var checkmark = document.createElementNS(svgns, 'text');
      var size = userSvg.clientWidth / 2;
      checkmark.setAttributeNS(null, 'x', size / 2.5);
      checkmark.setAttributeNS(null, 'y', size * 1.25);
      checkmark.setAttributeNS(null, 'font-size', size);
      var textNode = document.createTextNode("✅");
      checkmark.appendChild(textNode);
      userSvg.appendChild(checkmark);
      setTimeout(function() {
        fadeOut(checkmark);
      }, 1000);
    }
    setTimeout(function() {
      app.ports.returnCheckResultPort.send(isAllGood);
    }, 2000);
  });
});

function fadeOut(element) {
  var op = 1;  // initial opacity
  var timer = setInterval(function () {
      if (op <= 0.1){
          clearInterval(timer);
          element.remove();
      }
      element.style.opacity = op;
      element.style.filter = 'alpha(opacity=' + op * 100 + ")";
      op -= op * 0.1;
  }, 40);
}

function findNearestNeighbor(polylines, point) {
  var nearestDistance = Infinity;
  var nearestNeighbor;

  polylines.forEach(function(line) {
    line.forEach(function(neighbor) {
      var d = Math.sqrt(Math.pow(neighbor[0] - point[0], 2) + Math.pow(neighbor[1] - point[1], 2));
      if(d < nearestDistance) {
        nearestDistance = d;
        nearestNeighbor = neighbor;
      }
    });
  });

  return { neighbor: nearestNeighbor, distance: nearestDistance };
}

// extract strokes only and convert to black-and-white
function preprocessForTracer(imgData) {
  const data = imgData.data;

  // enumerate all pixels
  // each pixel's r,g,b,a datum are stored in separate sequential array elements
  for (let i = 0; i < data.length; i += 4) {
    const red = data[i];
    const green = data[i + 1];
    const blue = data[i + 2];
    if (red <= 10 && green <= 10 && blue <= 10 && data[i+3] > 0) {
      // flip black stroke to white
      data[i] = 255;
      data[i + 1] = 255;
      data[i + 2] = 255;
      data[i + 3] = 255;
    } else {
      // set to black background
      data[i] = 0;
      data[i + 1] = 0;
      data[i + 2] = 0;
      data[i + 3] = 255;
    }
  }
}

app.ports.loadFontPort.subscribe(function (name) {
  const dir = process.env.PUBLIC_URL + "/fonts";
  let url = `url(${dir}/${name}.woff2) format("woff2"), url(${dir}/${name}.woff) format("woff"), url(${dir}/${name}.ttf) format("truetype")`;
  const font = new FontFace(name, url);
  font.load().then(function (loaded_font) {
    document.fonts.add(loaded_font);
  }).catch(function (error) {
    console.log(error);
  });
});

app.ports.replayCsldCharacterPort.subscribe(function () {
  document.getElementById("csld-character").getElementsByTagName("img")[0].src = csldCharacterUrl;
});

app.ports.loadCsldCharacterPort.subscribe(function ([scriptType, char]) {
  fetch(`${corsProxy}https://www.moedict.tw/api/web/word/${char}`)
    .then(function (response) {
      return response.json();
    })
    .then(function (moeJson) {
      if (moeJson === undefined || moeJson.data === undefined) {
        setCsldCharacterUrl("");
        return;
      }
      const charInfo = moeJson.data.strokes.find(function (element) {
        return element.key === scriptType;
      });
      if (charInfo !== undefined) {
        fetch(corsProxy + charInfo.gif)
          .then(function (response) { return response.blob(); })
          .then(function (blob) {
            convertBlobToBase64(blob).then(function (charUrl) {
              csldCharacterUrl = charUrl;
              fetch(corsProxy + charInfo.jpg)
                .then(function (response) { return response.blob(); })
                .then(function (blob) {
                  convertBlobToBase64(blob).then(function (staticCharUrl) {
                    staticCsldCharacterUrl = staticCharUrl;
                    app.ports.setCsldCharacterUrlPort.send(staticCharUrl);
                  });
                })
            });
          })
      } else {
        setCsldCharacterUrl("");
      }
    });
});

function setCsldCharacterUrl(url) {
  csldCharacterUrl = url;
  app.ports.setCsldCharacterUrlPort.send(url);
}

// Converts any given blob into a base64 encoded string.
function convertBlobToBase64(blob) {
  return new Promise(function (resolve, reject) {
    const reader = new FileReader();
    reader.onerror = reject;
    reader.onload = function () {
      resolve(reader.result);
    };
    reader.readAsDataURL(blob);
  });
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
