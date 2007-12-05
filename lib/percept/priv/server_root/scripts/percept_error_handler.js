var onerror=handleErr;

function handleErr(msg,url,l) {
	var txt = "Error: " + msg + "\nURL: " + url + "\nCode line: " + l;
	alert(txt);
}
