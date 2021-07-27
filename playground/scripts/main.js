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
