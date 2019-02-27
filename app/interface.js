const AjaxHandler = new Intercafe('AjaxHandler', ['request', 'createXhr']);

class SimpleHandler {
    constructor() {
        this.createXhr = null;
    }
    request(method, url, callback, postVars) {
        const xhr = this.createXhr();
        xhr.onreadystatechange = function () {
            if (xhr.readyState !== 4) return;
            xhr.status === 200 ? callback.success(xhr.responseText, xhr.responseXML) : callback.failure(xhr.status);
        }
        xhr.open(method, url, true);
        if (method !== 'POST') postVars = null;
        xhr.send(postVars);
    }
    createXhr() {
        const methods = [
            function () { return new XMLHttpRequest(); },
            function () { return new ActiveXObject('Msxml2.XMLHTTP'); },
            function () { return new ActiveXObject('Microsoft.XMLHTTP'); }
        ];
        for (const method of methods) {
            try {
                method();
            } catch (e) {
                continue;
            }
            this.createXhr = method; // memoize the method
            return method;
        }
        throw new Error('SimpleHandler：Can not create an xhr object.');
    }
}

class QueueHandler extends SimpleHandler {
    constructor() {
        this.queue = [];
        this.requestInProcess = false;
        this.retryDelay = 5;
    }
    advanceQueue() {
        if (this.queue.length === 0) {
            this.requestInProcess = false;
            return;
        }
        const req = this.queue.shift();
        this.request(req.method, req.url, req.callback, req.postVars, true);
    }
    request(method, url, callback, postVars, override) {
        if (this.requestInProcess && !override) {
            this.queue.push({ method, url, callback, postVar });
        } else {
            this.requestInProcess = true;
            const xhr = this.createXhr();
            xhr.onreadystatechange = function () {
                if (xhr.readyState !== 4) return;
                if (xhr.status === 200) {
                    callback.success(xhr.responseText, xhr.responseXML)
                    this.advanceQueue();
                } else {
                    callback.failure(xhr.status);
                    setTimeout(() => {
                        this.request(method, url, callback, postVars, true);
                    }, this.retryDelay * 1000);
                }
            }
            xhr.open(method, url, true);
            if (method !== 'POST') postVars = null;
            xhr.send(postVars);
        }
    }
}

class OfflineHandler extends SimpleHandler {
    constructor() {
        this.storedRequests = [];
    }
    request(method, url, callback, postVars) {
        if (XhrManager.isOffline()) {
            this.storedRequests.push({ method, url, callback, postVar });
        } else {
            // online的时候使用父类发送xhr
            this.flushStoredRequests();
            super.request(method, url, callback, postVars);
        }
    }
    flushStoredRequests() {
        for (const req of this.storedRequests) {
            super.request(req.method, req.url, req.callback, req.postVars);
        }
    }
}

const XhrManager = {
    createXhrHandler() {
        let xhr;
        if (this.isOffline()) {
            xhr = new OfflineHandler();
        } else if (this.isHighLatency()) {
            xhr = new QueueHandler();
        } else {
            xhr = new SimpleHandler();
        }
        Intercafe.ensureTmplements(xhr, AjaxHandler);
        return xhr;
    },
    isOffline() { },
    isHighLatency() { }
};