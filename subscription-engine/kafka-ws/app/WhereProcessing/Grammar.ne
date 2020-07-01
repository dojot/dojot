@{%
const Lexer = require('./Lexer');
%}

@lexer Lexer

where
  -> null
  | expression {% id %}

expression
  -> condition:+ {% id %}

condition
  -> %parameter %operator:? %values
  {%
    function(data) {
      // Returns the values directly instead of all the parser things
      return {
        parameter: data[0].value,
        operator: data[1] ? data[1].value : 'eq',
        values: data[2].value,
      }
    }
  %}
