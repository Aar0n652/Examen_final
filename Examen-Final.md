Examen final
================
Cesar Quiroz
31/1/2022

## 11. En hidrología el tiempo de concentración (tc) representa el tiempo de viaje de una gota de lluvia que cae en el punto hidráulicamente más alejado de la cuenca y escurre superficialmente hasta su salida. Existen diferentes fórmulas, sobretodo empíricas, para el cálculo del *tc*. Crear una Función en R para el cálculo del *tc* según la siguiente fórmula.

``` r
L <- 1200
Cs <- 190
Ci <- 80
Tc <- function(L, Cs, Ci){
  0.3*(L/(((Cs+Ci)/2)^0.25))^0.76
}
Tc(L, Cs, Ci)
```

    ## [1] 25.85447

## 12. El dataset *airquality* contiene información sobre la velocidad de viento promedio en Milla/hora, realizar la clasificación de acuerdo a la Escala Beaufort y Douglas

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.1.2

    ## Warning: package 'forcats' was built under R version 4.1.2

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
data(airquality)
airquality_mph <- airquality %>%
  select(Wind)
class(airquality_mph)
```

    ## [1] "data.frame"

``` r
class(airquality_mph['Wind'])
```

    ## [1] "data.frame"

``` r
for(atmp in airquality_mph['Wind']) { airquality_mph_vec <- atmp }
class(airquality_mph_vec)
```

    ## [1] "numeric"

``` r
for(mph in airquality_mph_vec){
  if(mph >= 64){
    print("Temporal huracanado - Enorme")
  }else if( mph >= 56 & mph < 64){
    print("Temporal muy duro")
  }else if( mph >= 48 & mph < 56){
    print("Temporal duro - Montañosa")
  }else if( mph >= 41 & mph < 48){
    print("Fuerte temporal")
  }else if( mph >= 34 & mph < 41){
    print("Temporal - Arbolada")
  }else if( mph >= 28 & mph < 34){
    print("Frescachón - Muy gruesa")
  }else if( mph >= 22 & mph < 28){
    print("Fresco - Gruesa")
  }else if( mph >= 17 & mph < 22){
    print("Fresquito - Fuerte marejada")
  }else if( mph >= 11 & mph < 17){
    print("Moderado - Marejada")
  }else if( mph >= 7 & mph < 11){
    print("Flojo - Marejadilla")
  }else if( mph >= 4 & mph < 7){
    print("Flojito")
  }else if( mph >= 1 & mph < 4){
    print("Ventolina - Rizada")
  }else if( mph >= 0 & mph < 1){
    print("Calma - Llana")
  }
}
```

    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Fresquito - Fuerte marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Fresquito - Fuerte marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Fresquito - Fuerte marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Ventolina - Rizada"
    ## [1] "Flojito"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojito"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojito"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Ventolina - Rizada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Ventolina - Rizada"
    ## [1] "Flojito"
    ## [1] "Flojito"
    ## [1] "Flojito"
    ## [1] "Flojito"
    ## [1] "Ventolina - Rizada"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojito"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojito"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojito"
    ## [1] "Moderado - Marejada"
    ## [1] "Moderado - Marejada"
    ## [1] "Flojo - Marejadilla"
    ## [1] "Moderado - Marejada"

## Adicional. A partir de la exposición realizada en clase, realizar un resumen del paquete expuesto por su grupo en su repositorio en github en formato markdown

``` r
#GGPLOT2 se encuentra dentro de la libreria tidyverse o tambien se puede instalar este paquete a parte, 
#donde esta libreria de GGPLOT2 nos permite desarrollar gráficos interactivos de manera rapida, comoda y eficiente.
#1) Tendriamos la parte de ggplot() donde se pondría la data y si se desea las esteticas pero estos tambien pueden estar
# en la parte de las geometrias o diagramas.
#2) Tenemos la parte de las geometrias o diagramas como geom_point(), geom_line(), geom_violin, geom_histogram(), ect.
#3) Luego de las esteticas, seguiria el sistema de coordenadas o los labs que van en los ejes x e y.
#4) Podemos agregar un titulo con ggtitle()
#5) Luego las facetas con facet_grid() donde nos da la posibilidad de hacer varios graficos en uno solo.
#6) Podemos agregar la parte de estadisticas con stat_summary, stat_smooth(), etc.
#7) Por último le agregamos los temas como theme_classic(), theme_dark(), etc.
```
