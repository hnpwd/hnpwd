function srand (seed) {
  let x = seed
  return function () {
    x = (1664525 * x + 1013904223) % 4294967296
    return x
  }
}

function shuffle (items) {
  const rand = srand(Math.floor(Date.now() / 1000 / 3600))
  items = items.slice()
  for (let n = items.length - 1; n >= 1; n--) {
    let m = rand() % n
    ;[items[m], items[n]] = [items[n], items[m]]
  }
  return items
}

function shuffleCards () {
  let cards = Array.from(document.querySelectorAll('section'))
  document.querySelector('main').replaceChildren(...shuffle(cards))
  window.setTimeout(shuffleCards, 60000)
}

window.addEventListener('load', shuffleCards)
