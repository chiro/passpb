function sethtml(domain) {
    var password = document.getElementById('password');
    password = password.value;

    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/scramble?pass=' + password + '&realm=' + domain, true);
    xhr.onreadystatechange = function() {
        if (xhr.readyState === 4 && xhr.status === 200) {
            var field = document.getElementById('serv-' + domain);
            field.value = xhr.responseText;
        }
    };
    xhr.send(null);
}
