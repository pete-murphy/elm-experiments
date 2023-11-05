import "../css/main.css";
import { Elm } from "../elm/Main.elm";

// Start the Elm application.
const app = Elm.Main.init({
	node: document.querySelector("main"),
	flags: "{}",
});
