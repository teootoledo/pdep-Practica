object nico {
	method cuantoCobra(materia) = materia.length() * 50
}

object carlono {
	var precio = 300
	
	method precio(nuevoPrecio){
		precio = nuevoPrecio
	}
	
	method cuantoCobra(materia) = precio 
}

object camila {
	var estaDeBuenHumor = true
	method cuantoCobra(materia) = if (estaDeBuenHumor) 250 else 700
}