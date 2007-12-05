
function selectall() {
    for (var i = 0; i < document.process_select.elements.length; i++) {
	var e = document.process_select.elements[i];
	if ((e.name != 'select_all') && (e.type == 'checkbox')) {
		e.checked = document.process_select.select_all.checked;
	}
    }
}
