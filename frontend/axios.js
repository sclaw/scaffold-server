function validateOnBlur(e, inputGroup, formBlockId) {
	var validationInstance = $('#'+formBlockId).parsley();
	console.log(validationInstance)
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
	let form = document.querySelector('#loginblock');
	let formData = new FormData(form);
	let validationInstance = $('#loginblock').parsley();
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
				console.log('sukses login!')
/* 				Cookies.set('name', 'value', { expires: 7, path: '/' });
				console.log(Cookies.get('name')) */
				localStorage.setItem('accessToken', response.data.success.accessToken);
				localStorage.setItem('refreshToken', response.data.success.refreshToken);
				document.location.href = '/app/'
			} else if (response.data.error.client) {
				console.log('login parol neverniy!')
				showInValidError(inputLogInName)
				showInValidError(inputLogInPass)
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

function register() {
	let regForm = document.querySelector('#registry_panel');
	let regFormData = new FormData(regForm);
	let validationInstance = $('#registry_panel').parsley();
	console.log(validationInstance.isValid())
	if (validationInstance.isValid()) {
		axios({
			method: 'post',
			url: 'http://109.228.61.185:12000/api/v1/auth/registration',
			data: {
				'email': regFormData.get('email'),
				'password': regFormData.get('password'),
				'login':  regFormData.get('login')
			},
			headers: {
				'accept': 'application/json;charset=utf-8',
				'content-type': 'application/json;charset=utf-8',
			},
			crossDomain: true,
		}).then( function (response) {
			console.log(response);
			if (response.data.success) {
				 console.log('sukses register!')
				 document.location.href = '/app/'
				 localStorage.setItem('accessToken', response.data.success.accessToken);
				 localStorage.setItem('refreshToken', response.data.success.refreshToken); 
			} else if (response.data.error) {
				console.log (response.data.error)
			}
		})
	}

}

function test() {
	console.log('test')
}

window.onload = function () {
	//Login validation
	inputLogInName = document.querySelector('#loginName')
	inputLogInPass = document.querySelector('#loginPass')
	inputLogInName.onblur = function() {
		validateOnBlur(inputLogInName, 'first', 'loginblock')
	};
	inputLogInPass.onblur = function() {
		validateOnBlur(inputLogInPass, 'second', 'loginblock')
	};
	inputLogInName.onfocus = function() {
		inputLogInName.classList.remove('not-valid');
	};
	inputLogInPass.onfocus = function() {
		inputLogInPass.classList.remove('not-valid');
	};
	//Register validation
	inputRegName = document.querySelector('#regName')
	inputRegName.onblur = function() {
		validateOnBlur(inputRegName, 1, 'registry_panel')
		console.log('hey')
	}
	inputRegName.onfocus = function() {
		inputRegName.classList.remove('not-valid');
	};
	inputRegMail = document.querySelector('#regMail')
	inputRegMail.onblur = function() {
		validateOnBlur(inputRegMail, 2, 'registry_panel')
		console.log('hey')
	}
	inputRegMail.onfocus = function() {
		inputRegMail.classList.remove('not-valid');
	};
	inputRegPass = document.querySelector('#regPass')
	inputRegPass.onblur = function() {
		validateOnBlur(inputRegPass, 3, 'registry_panel')
		console.log('hey')
	}
	inputRegPass.onfocus = function() {
		inputRegPass.classList.remove('not-valid');
	};
	inputRegPassRepeat = document.querySelector('#regPassRepeat')
	inputRegPassRepeat.onblur = function() {
		validateOnBlur(inputRegPassRepeat, 4, 'registry_panel')
		console.log('hey')
	}
	inputRegPassRepeat.onfocus = function() {
		inputRegPassRepeat.classList.remove('not-valid');
	};
	/* inputRegName.addEventListener("onblur", function() {
		validateOnBlur(inputRegName, 1, 'registry_panel')
		console.log('hey')
	}) */
	inputRegMail = document.querySelector('#regMail')
	inputRegPass = document.querySelector('#regPass')
	inputRegPassRepeat = document.querySelector('#regPassRepeat')
	inputRegPassCheck = document.querySelector('#regCheck')
	inputRegFields = document.querySelectorAll('#regName,#regMail,#regPass,#regPassRepeat,#regCheck')
	console.log(inputRegFields)
	let numArr = ['first', 'second', 'third', 'fourth', 'fifth']
	for ( var i = 0; i < inputRegFields.length; i++) {
		/* inputRegFields[i].onblur = function() {
			validateOnBlur(inputRegFields[i], numArr[i+1], 'registry_panel')
			console.log('hey')
		} */ 
		/* inputRegFields[i].addEventListener("onblur", function() {
			validateOnBlur(inputRegFields[i], numArr[i+1], 'registry_panel')
			console.log('hey')
		}) */
	}
	/* inputRegName.onblur = function() {
		validateOnBlur(inputRegName, 'first', 'loginblock')
	}; */
	
	//localStorage.setItem('myCat', 'Tom');
	//console.log(localStorage.getItem('myCat'))
	//console.log('hey')
}
