import './main.css';
import './elm-pep';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.loadFont.subscribe(function (name) {
  const dir = process.env.PUBLIC_URL + "/fonts";
  let url = `url(${dir}/${name}.woff2) format("woff2"), url(${dir}/${name}.woff) format("woff"), url(${dir}/${name}.ttf) format("truetype")`;
  const font = new FontFace(name, url);
  font.load().then(function (loaded_font) {
    document.fonts.add(loaded_font);
  }).catch(function (error) {
    console.log(error);
  });
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
