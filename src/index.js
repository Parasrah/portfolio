import { Elm } from './Main.elm'

window.addEventListener('load', () => {
  const app = Elm.Main.init({
    node: document.getElementsByTagName('body')[0],
    flags: {
      width: window.innerWidth,
      height: window.innerHeight,
    },
  })
})
