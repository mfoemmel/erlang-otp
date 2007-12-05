    function init_load_image() {
	var percept_graph = document.getElementById("percept_graph");
	if (percept_graph) {
		var percept_middle = document.getElementById("percept_middle");
		var percept_graph_loading = document.getElementById("percept_graph_loading");
		var width = percept_middle.offsetWidth;
		percept_graph_loading.style.visibility = "hidden";
		percept_graph.style.visibility = "visible";
//		percept_graph.style.width = width - 40;
	}
    }

    function select_image() {
	var Graph = document.getElementById("percept_graph");
	if (Graph) {
	    var GraphIndex = document.form_area.graph_select.selectedIndex;
	    var GraphSelectValue = document.form_area.graph_select.options[GraphIndex].value;
	    Graph.style.backgroundImage = "url('" + GraphSelectValue +"')";
	}
    }
   
   function select_down(event) {
	var Graf = document.getElementById("percept_graph");
        var Area = document.getElementById("percept_areaselect");
        var x =  event.offsetX?(event.offsetX):event.pageX-Graf.offsetLeft;
        
	// Trim edges
	
	if ( x < 20 ) {
	    x = 20;
	}

        if ( x > 680 ) {
	    x = 680;
	}
        var width = Graf.offsetWidth;
        var Xmin = document.form_area.data_min.value;
        var Xmax = document.form_area.data_max.value;
        Area.style.left = x;
        Area.style.top = Graf.offsetHeight - 20;
        Area.style.width = 1;
        Area.style.height = 10;
        Area.moving = true;
        Area.bgcolor = "#00ff00";
	Area.style.visibility = "visible";
	//var RangeMin = (( (x - 20)* (Xmax - Xmin)/(width - 40)) + Xmin*1);
	var RangeMin = convert_image2graph(x, Xmin, Xmax, 20, 680);
        Area.style.opacity = 0.5;
        Area.style.filter = 'alpha(opacity=50)';
        if (RangeMin == 0) document.form_area.range_min.value = 0.0;
	else document.form_area.range_min.value = RangeMin;
    }

    function select_move(event) {
        var Graf = document.getElementById("percept_graph");
        var Area = document.getElementById("percept_areaselect");
        var x =  event.offsetX?(event.offsetX):event.pageX-Graf.offsetLeft;
        if (Area.moving == true) {
	    
	    // Trim edges
	
	    if ( x < 20 ) {
	    	x = 20;
	    }

            if ( x > 680 ) {
	    	x = 680;
	    }

	    var width = Graf.offsetWidth;
            var Xmin = document.form_area.data_min.value;
            var Xmax = document.form_area.data_max.value;
            var x0 = min(x, Area.offsetLeft);
            var x1 = max(x, Area.offsetLeft);
            var w = (x1 - x0);
            Area.style.left = x0;
            Area.style.width = w;
	var RangeMin = convert_image2graph(x0, Xmin, Xmax, 20, 680);
	var RangeMax = convert_image2graph(x1, Xmin, Xmax, 20, 680);
//	    var RangeMin = (((x0 - 20)*(Xmax - Xmin)/(width - 20)) + Xmin*1.0);
//	    var RangeMax = (((x1 - 20)*(Xmax - Xmin)/(width - 20)) + Xmin*1.0);
            Area.style.opacity = 0.5;
	    Area.style.visibility = "visible";
           
	    if (RangeMin == 0) document.form_area.range_min.value = 0.0;
	    else document.form_area.range_min.value = RangeMin;
            if (RangeMax == 0) document.form_area.range_max.value = 0.0;
	    else document.form_area.range_max.value = RangeMax;
	}
    }
    function select_up(event) {
        var Graf = document.getElementById("percept_graph");
        var Area = document.getElementById("percept_areaselect");
        var x =  event.offsetX?(event.offsetX):event.pageX-Graf.offsetLeft;
	// Trim edges
	
	if ( x < 20 ) {
	    x = 20;
	}

        if ( x > 680 ) {
	    x = 680;
	}
	var width = Graf.offsetWidth;
	var w = (x - Area.style.offsetLeft);
        var Xmin = document.form_area.data_min.value;
        var Xmax = document.form_area.data_max.value;
        Area.moving = false;
        Area.style.width = w;
	var RangeMax = convert_image2graph(x, Xmin, Xmax, 20, 680);
//        var RangeMax = (((x - 20)*(Xmax - Xmin)/(width - 40)) + Xmin*1.0);
        if (RangeMax == 0) document.form_area.range_max.value = 0.0;
	else document.form_area.range_max.value = RangeMax;
    }
    function min(A, B) {
        if (A > B) return B;
        else return A;
    }
    function max(A,B) {
        if (A > B) return A;
        else return B;
    }
    function convert_image2graph(X, Xmin, Xmax, X0, X1) {
	var ImageWidth = X1 - X0;
	var RangeWidth = Xmax - Xmin;
	var DX = RangeWidth/ImageWidth;
	var Xprime = (X - X0)*DX + Xmin*1.0;
	return Xprime;
    }
//	var RangeMin = (((x0 - 20)*(Xmax - Xmin)/(width - 20)) + Xmin*1.0);
