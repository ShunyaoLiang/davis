async function run_code() {
	// Get Pseudocode from editor.
	let editor = document.getElementById('user-input');
	let pseudocode = editor.textContent.trim();
	// Send the buffer contents of the editor to the evaluator and store
	// the result.
	const result = await fetch(window.location.href.split('#')[0] + 'evaluate/', {
		method: 'POST',
		headers: {
			'Content-Type': 'text/plain',
		},
		body: pseudocode,
	})
	.then(response => { return response.body.getReader().read(); })
	.then(object => { return new TextDecoder().decode(object.value); })
	// Display the result.
	let output = document.getElementById('output');
	output.textContent = result;	

	console.log(result);
}

$(document).ready(function() {
	$('#user-input').keydown(function(event) {
		// Get the character to insert.
		let c;
		switch (event.key) {
			case 'Tab':
				c = '\t';
				break;
			case 'Enter':
				c = '\n';
				break;
			// If it is not one of the above cases, default behaviour is fine.
			default:
				return;
		}
		// Otherwise, browsers will insert <br> or <div> on Enter and change focus on Tab.
		event.preventDefault();
		// Insert the text.
		let text = this.textContent.trim(),
			position = $('#user-input').caret('pos'),
			left = text.substring(0, position)
		    right = text.substring(position);
		this.textContent = left + c + right;
		// Increment the caret.
		$('#user-input').caret('pos', position + 1);
	})
})
