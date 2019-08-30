require('child_process').execSync('elm make src/Main.elm --output=elm.js', {
  stdio: 'inherit',
})
