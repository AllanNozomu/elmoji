import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

let app = Elm.Main.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

toastr.options.timeOut = 2000;

app.ports.copy.subscribe((emoji) => {
  navigator.clipboard.writeText(emoji);
  toastr.success(`${emoji} coppied to clipboard`)
});