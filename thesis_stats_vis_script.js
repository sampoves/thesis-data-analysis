function show_hide(shrink_me, parent_div) {
	var elem_to_hide = document.getElementById(shrink_me);
	var parent_div = '#' + parent_div;

	if(elem_to_hide.style.display === 'none') {
		elem_to_hide.style.display = 'block';
		$(parent_div).find('i.icon.eye').toggleClass('eye eyeslash');
		$(parent_div).find('i.icon.eyeslash')[0].setAttribute('title', 'Hide element');
	} else {
		elem_to_hide.style.display = 'none';
		$(parent_div).find('i.icon.eyeslash').toggleClass('eyeslash eye');
		$(parent_div).find('i.icon.eye')[0].setAttribute('title', 'Show element');
	}
};