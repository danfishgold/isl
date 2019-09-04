const execSync = require('child_process').execSync

try {
  console.log(execSync('elm make src/Main.elm --output=elm.js').toString())
} catch (err) {
  if (err.message.includes('elm-stuff') || err.message.includes('/.elm')) {
    execSync('rm -r elm-stuff')
    execSync('rm -r .elm')
    console.log(execSync('elm make src/Main.elm --output=elm.js').toString())
  } else {
    throw err
  }
}
