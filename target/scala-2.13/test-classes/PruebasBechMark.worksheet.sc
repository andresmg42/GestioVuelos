import Datos._
import Itinerarios._
import ItinerariosPar._
import Benchmark._


val its200Sec = itinerarios(vuelosC1++vuelosC2, aeropuertos)
val its200Par = itinerariosTiempo(vuelosC1++vuelosC2, aeropuertos)

//compararAlgoritmos3(its200Sec,its200Par)("ORD","TPA")


val itSalidaCurso = itinerarioSalida(vuelosCurso, aeropuertosCurso)
val itSal1 = itSalidaCurso("CTG", "PTY", 11, 40)
val itSal2 = itSalidaCurso("CTG", "PTY", 11, 55)
val itSal3 = itSalidaCurso("CTG", "PTY", 10, 30)



