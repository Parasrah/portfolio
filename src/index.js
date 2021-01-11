import { Elm } from './Main.elm'

window.addEventListener('load', () => {
  const app = Elm.Main.init({
    node: document.getElementsByTagName('body')[0],
    flags: {
      dimensions: {
        scene: {
          height: document.body.scrollHeight,
          width: document.body.scrollWidth,
        },
        viewport: {
          height: window.innerHeight,
          width: window.innerWidth,
          x: document.body.scrollLeft,
          y: document.body.scrollTop,
        },
      },
    },
  })

  document.addEventListener('scroll', function () {
    app.ports.onScroll.send('scroll')
  })
})
