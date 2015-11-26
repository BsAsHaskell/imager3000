imager3000
==========

Un simple scrapper de imágenes concurrente.

Llamando `imager3000 "http://haskell.com"` nos baja las imágenes
de la URL en `./_downloaded/`

## Diseño

Esta diseñado a partir de modularizar la concurrencia, ofreciendo por el momento
dos implementaciones para la misma. Ambas exponen una función:

```
concurrently :: Config -> [IO ()] -> IO ()
```

que toma cuantos workers usar (default 5), una lista de "cosas"
y una función que toma esas "cosas" y hace IO.

`Imager3000.Concurrent.MVar` usa un truquito con MVars,
que son la unidad de concurrencia básica de Haskell.

`Imager3000.Concurrent.Async` en cambio usa una librería que hace todo solo.

`Imager3000.Fetch` es donde deberiamos programar la descarga de imágenes,
y es lo que haremos en el workshop.

### Cómo instalar GHC

https://github.com/BsAsHaskell/workshop-1/blob/master/README.md
