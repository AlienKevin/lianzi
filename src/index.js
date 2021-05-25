import './main.css';
import './elm-pep';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

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

app.ports.loadCsldCharacterPort.subscribe(function ([scriptType, char]) {
  fetch(`https://www.moedict.tw/api/web/word/${char}`)
  .then(function(response) {
    return response.json();
  })
  .then(function(moeJson) {
    if (moeJson === undefined || moeJson.data === undefined) {
      app.ports.setCsldCharacterUrlPort.send("");
      return;
    }
    const charInfo = moeJson.data.strokes.find(function(element) {
      return element.key === scriptType;
    });
    if (charInfo !== undefined) {
      fetch(charInfo.gif)
      .then(function(response) { return response.blob();})
        .then(function(blob) {
            convertBlobToBase64(blob).then(function(charUrl) {
              app.ports.setCsldCharacterUrlPort.send(charUrl);
            });
        })
    } else {
      app.ports.setCsldCharacterUrlPort.send("");
    }
  });
});

// Converts any given blob into a base64 encoded string.
function convertBlobToBase64(blob) {
  return new Promise(function(resolve, reject) {
    const reader = new FileReader();
    reader.onerror = reject;
    reader.onload = function() {
      resolve(reader.result);
    };
    reader.readAsDataURL(blob);
  });
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
