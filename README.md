# 🍎 CadregraScript - IN DEVELOPMENT!

A totally useless **pseudo** programming language made **just for fun**

![L'inganno della cadrega](https://scontent-mxp1-1.xx.fbcdn.net/v/t1.0-9/10924750_10152651904133181_3882763954929450692_n.jpg?_nc_eui2=v1%3AAeFEGOlF3UD1GcDYOmLa51V-GVMCEPE0awKmp2HY3gnwj1fLyHqLSDAyaWMLj8EGk5qUbxvNGOfo0za28SnB-pOH-sVGZX-BUXruL6bvwFxlBQ&oh=26d9bcbae6d3ae84784618d92a168c84&oe=5B3D17BA)

# Motivation

There's not a valid motivation. I just wanted to do something stupid and waste my free time because I can't sleep.

The entire language is in the **maccheroni idioma** (sorry people), so only italians will be able to understand (maybe) what's going on. Sorry again.

# Usage

Here you are some examples:

**CadregaScript**
```cadrega
  acchiappa nomeModulo da('./nomemodulo.cadrega')

  conte foo è 'Questa è una costante!'
  lascia che bar sia 'Questa è una variabile!'

  stai zitto! Esempio di array
  conte array = ['a', 'b', 'c', 'd']

  vai di ciclo (lascia che i sia di array){
    sputa(i)
  }

```

compiles the following CommonJs output:
```js
  const nomeModulo = require('./nomemodulo')

  const foo = 'Questa è una costante!'
  let bar = 'Questa è una variabile!'

  // Esempio di array
  const array = ['a', 'b', 'c', 'd']

  for(let i of array){
    console.log(i)
  }

```

# License

CadregaScript is licensed under [MIT License](/LICENSE)
