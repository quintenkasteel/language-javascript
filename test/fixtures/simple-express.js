// Simple Express.js-style server without require/import
function createExpressApp() {
  var app = {};
  var routes = {};
  var middleware = [];
  
  app.use = function(handler) {
    middleware.push(handler);
  };
  
  app.get = function(path, handler) {
    routes[path] = routes[path] || {};
    routes[path].GET = handler;
  };
  
  app.post = function(path, handler) {
    routes[path] = routes[path] || {};
    routes[path].POST = handler;
  };
  
  app.listen = function(port, callback) {
    console.log('Server listening on port ' + port);
    if (callback) {
      callback();
    }
  };
  
  return app;
}

// Usage example
var app = createExpressApp();

app.get('/', function(req, res) {
  res.json({ message: 'Hello World!' });
});

app.get('/api/users/:id', function(req, res) {
  var id = req.params.id;
  var user = { id: id, name: 'User ' + id };
  res.json(user);
});

app.post('/api/users', function(req, res) {
  var name = req.body.name;
  var email = req.body.email;
  
  if (!name || !email) {
    res.status(400).json({ error: 'Name and email required' });
    return;
  }
  
  var newUser = {
    id: Date.now(),
    name: name,
    email: email,
    createdAt: new Date().toISOString()
  };
  
  res.status(201).json(newUser);
});

app.listen(3000, function() {
  console.log('Server started successfully');
});

if (typeof module !== 'undefined' && module.exports) {
  module.exports = createExpressApp;
}