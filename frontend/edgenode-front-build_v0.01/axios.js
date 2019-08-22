function validateOnBlur(e, inputGroup) {
/* 	var form = document.querySelector('#loginblock');
	var formData = new FormData(form); */
	var validationInstance = $('#loginblock').parsley();
	console.log(validationInstance.isValid({group:inputGroup}))
	if(!validationInstance.isValid({group:inputGroup})) {
		showInValidError(e)
	}
}

function showInValidError(e) {
	e.classList.add('not-valid', 'animated', 'shake');
	setTimeout(function() { e.classList.remove('animated', 'shake');}, 1000);
}

function login() {	
	var form = document.querySelector('#loginblock');
	var formData = new FormData(form);
	var validationInstance = $('#loginblock').parsley();
	console.log(validationInstance.isValid())
	if (validationInstance.isValid()) {
		axios({
			method: 'post',
			url: 'http://109.228.61.185:12000/api/v1/auth/signin',
			// url: 'http://localhost:3000/api/v1/auth/signin',
			data: {
				'email': formData.get('email'),
				'password': formData.get('password')
			}, 
			headers: {
				'accept': 'application/json;charset=utf-8',
				'content-type': 'application/json;charset=utf-8',
			},
			crossDomain: true,
			/* withCredentials: true */
		}).then(function (response) {
			// handle success
			console.log(response);
			if (response.data.success) {
				console.log('sukses!')
/* 				Cookies.set('name', 'value', { expires: 7, path: '/' });
				console.log(Cookies.get('name')) */
				localStorage.setItem('accessToken', response.data.success.accessToken);
				localStorage.setItem('refreshToken', response.data.success.refreshToken);
				document.location.href = '/app/'
			} else if (response.data.error.client) {
				console.log('login parol neverniy!')
				showInValidError(inputName)
				showInValidError(inputPass)
			}
		})
		.catch(function (error) {
			// handle error
			console.log(error);
		})
		.finally(function () {
			// always executed
		});
	} else {
		showInValidError(inputName)
		showInValidError(inputPass)
	}
}

function test() {
	console.log('test')
}

var inputName

window.onload = function () {
	inputName = document.querySelector('#name')
	inputPass = document.querySelector('#pass')
	inputName.onblur = function() {
		validateOnBlur(inputName, 'first')
	};
	inputPass.onblur = function() {
		validateOnBlur(inputPass, 'second')
	};
	inputName.onfocus = function() {
		inputName.classList.remove('not-valid');
	};
	inputPass.onfocus = function() {
		inputPass.classList.remove('not-valid');
	};
	//localStorage.setItem('myCat', 'Tom');
	//console.log(localStorage.getItem('myCat'))
	//console.log('hey')
}
