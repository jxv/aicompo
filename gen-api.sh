#! /bin/sh

yaml2json () {
  ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load($stdin.read))'
}


for i in api/tictactoe/*.yaml; do
    [ -f "$i" ] || break
    echo $i
    j=`basename $i .yaml`
    yaml2json < $i > "api-json/tictactoe/$j.json"
done


fluid -l haskell -s api-json/tictactoe -m CompoAi.Api.TicTacToe -n TicTacToe -d ./src/CompoAi/Api -e server -a scotty
