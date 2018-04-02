#! /bin/sh

yaml2json () {
  ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load($stdin.read))'
}

gen_api() {
  mkdir -p api-json/$1
  rm api-json/$1/*

  for yaml in api/$1/*.yaml; do
      [ -f "$yaml" ] || break
      echo $yaml
      json=`basename $yaml .yaml`
      yaml2json < $yaml > "api-json/$1/$json.json"
  done

  fluid -l haskell -s api-json/$1 -m CompoAi.Api.$2 -n $2 -d ./src/CompoAi/Api -e server -a scotty
}

gen_api "tictactoe" "TicTacToe"

# fluid -l haskell -s api-json/tictactoe -m CompoAi.Api.TicTacToe -n TicTacToe -d ./src/CompoAi/Api -e client -a http-client
