import Datos._
import Itinerarios._
import ItinerariosPar._
import Benchmark._


vuelosA1.length
vuelosA2.length
vuelosA3.length
vuelosA4.length
vuelosA5.length
vuelosB1.length
vuelosB2.length
vuelosB3.length
vuelosB4.length
vuelosB5.length
vuelosC1.length
vuelosC2.length
vuelosC3.length
vuelosC4.length
vuelosC5.length
vuelosD1.length

//val VuelosPrueba=(vuelosA1,vuelosA2,vuelosA3,vuelosA4,vuelosA5,vuelosB1,vuelosB2,vuelosB3,vuelosB4,vuelosB5,vuelosC1,vuelosC2,vuelosC3,vuelosC4,vuelosC5,A200,B200,vuelosC1++vuelosC2,vuelosC2++vuelosC3,vuelosC3++vuelosC4,vuelosD1)

val VuelosPrueba = Map(
  vuelosA1 -> ("HOU", "BNA"),
  vuelosA2 -> ("AA", "ORD"),
  vuelosA3 -> ("AA", "MSY"),
  vuelosA4 -> ("AA", "JFK"),
  vuelosA5 -> ("AA", "DFW"),
  vuelosB1 -> ("AA", "ORD"),
  vuelosB2 -> ("AA", "MSY"),
  vuelosB3 -> ("AA", "DFW"),
  vuelosB4 -> ("AS", "LAX"),
  vuelosB5 -> ("CO", "SEA"),
  vuelosC1 -> ("DL", "ATL"),
  vuelosC2 -> ("DL", "ATL"),
  vuelosC3 -> ("DL", "LAX"),
  vuelosC4 -> ("DL", "DFW"),
  vuelosC5 -> ("DL", "LAX")
)

val VuelosPrueba1 = Map(
  vuelosA1 -> ("HOU", "BNA"),
  vuelosB1 -> ("DFW", "ORD"),
  vuelosC1 -> ("ORD", "TPA"),
  B200->("ORD","TPA")
)

val VuelosPrueba2=Map(
  vuelosA1->("HOU","BNA"),
  vuelosB1->("DFW","ORD"),
  vuelosC1->("ORD","TPA"),
  vuelosC1++vuelosC2->("ORD","TPA"),
  vuelosC1++vuelosC2++vuelosC3++vuelosC4++vuelosC5->("ORD","TPA")

)

val vuelosP=Map(
  vuelosA1->("HOU","BNA"),
  vuelosA2->("HOU","BNA"),
  vuelosA3->("HOU","BNA"),
  vuelosA4->("HOU","BNA"),
  vuelosA5->("HOU","BNA"),
  vuelosB1->("DFW","ORD"),
  vuelosB2->("DFW","ORD"),
  vuelosB3->("DFW","ORD"),
  vuelosB4->("DFW","ORD"),
  vuelosB5->("DFW","ORD"),
  vuelosC1->("ORD","TPA"),
  vuelosC2->("ORD","TPA"),
  vuelosC3->("ORD","TPA"),
  vuelosC4->("ORD","TPA"),
  vuelosC5->("ORD","TPA")
 )

val VuelosPrueba3=Map(
  vuelosA1->("HOU","BNA"),
  vuelosA2->("HOU","BNA"),
  vuelosA3->("HOU","BNA"),
  vuelosA4->("HOU","BNA"),
  vuelosA5->("HOU","BNA"),
  vuelosB1->("DFW","ORD"),
  vuelosB2->("DFW","ORD"),
  vuelosB3->("DFW","ORD"),
  vuelosB4->("DFW","ORD"),
  vuelosB5->("DFW","ORD"),
  vuelosC1->("ORD","TPA"),
  vuelosC2->("ORD","TPA"),
  vuelosC3->("ORD","TPA"),
  vuelosC4->("ORD","TPA"),
  vuelosC5->("ORD","TPA"),
  vuelosC1++vuelosC2->("ORD","TPA"),
  vuelosC2++vuelosC3->("ORD","TPA"),
  vuelosC4++vuelosC5->("ORD","TPA"),
  vuelosC1++vuelosC3->("ORD","TPA"),
  vuelosC1++vuelosC4->("ORD","TPA"),
  vuelosC1++vuelosC2++vuelosC3->("ORD","TPA"),
  vuelosC2++vuelosC3++vuelosC4->("ORD","TPA"),
  vuelosC4++vuelosC5++vuelosC1->("ORD","TPA"),
  vuelosC2++vuelosC3++vuelosC5->("ORD","TPA"),
  vuelosC1++vuelosC3++vuelosC5->("ORD","TPA"),
  B200++vuelosC3++vuelosC4->("ORD","TPA"),
  B200++vuelosC2++vuelosC3->("ORD","TPA"),
  A200++vuelosC2++vuelosC3->("ORD","TPA"),
  vuelosC2++vuelosC3++A200->("ORD","TPA"),
  vuelosC1++vuelosC2++vuelosC3++vuelosC4->("ORD","TPA"),
  B200++vuelosC3++vuelosC4++vuelosC5->("ORD","TPA"),
  B200++vuelosC2++vuelosC3++vuelosC4->("ORD","TPA"),
  A200++vuelosC2++vuelosC3++vuelosC5->("ORD","TPA"),
  vuelosC1++vuelosC2++vuelosC3++A200->("ORD","TPA"),
  vuelosC1++vuelosC2++vuelosC3++vuelosC4++vuelosC5->("ORD","TPA")

)


/*val its200Sec = itinerariosTiempo(vuelosC1 ++ vuelosC2, aeropuertos)
val its200Par = itinerariosTiempoParCol(vuelosC1 ++ vuelosC2, aeropuertos)

val its300C =
  itinerariosTiempoPar(vuelosC1 ++ vuelosC2 ++ vuelosC3, aeropuertos)
val its300C2 =
  itinerariosTiempoParCol(vuelosC1 ++ vuelosC2 ++ vuelosC3, aeropuertos)

//compararAlgoritmos3(its200Sec,its200Par)("ORD","TPA",18,30)
*/

val its200Sec = itinerarios(vuelosC1, aeropuertos)
val its200Par = itinerariosPar(vuelosC1, aeropuertos)

crearPruebas(VuelosPrueba,aeropuertos,2,(itinerarios,itinerariosPar))

