// AMD (Asynchronous Module Definition) sample
define(['jquery', 'underscore', 'backbone'], function($, _, Backbone) {
  'use strict';
  
  // Simple AMD module
  const MyModel = Backbone.Model.extend({
    defaults: {
      name: '',
      age: 0,
      active: false
    },
    
    validate: function(attrs) {
      if (!attrs.name || attrs.name.trim() === '') {
        return 'Name is required';
      }
      if (attrs.age < 0) {
        return 'Age must be positive';
      }
    },
    
    toggle: function() {
      this.set('active', !this.get('active'));
    }
  });
  
  const MyCollection = Backbone.Collection.extend({
    model: MyModel,
    url: '/api/users',
    
    active: function() {
      return this.filter(model => model.get('active'));
    },
    
    inactive: function() {
      return this.filter(model => !model.get('active'));
    }
  });
  
  const MyView = Backbone.View.extend({
    tagName: 'div',
    className: 'user-list',
    
    events: {
      'click .toggle-user': 'toggleUser',
      'click .delete-user': 'deleteUser'
    },
    
    initialize: function() {
      this.listenTo(this.collection, 'add', this.addOne);
      this.listenTo(this.collection, 'reset', this.addAll);
      this.listenTo(this.collection, 'all', this.render);
    },
    
    render: function() {
      this.$el.html(this.template({ users: this.collection.toJSON() }));
      return this;
    },
    
    template: _.template(`
      <h2>Users (<%= users.length %>)</h2>
      <ul>
        <% _.each(users, function(user) { %>
          <li>
            <%= user.name %> (<%= user.age %>) 
            <span class="status"><%= user.active ? 'Active' : 'Inactive' %></span>
            <button class="toggle-user" data-id="<%= user.id %>">Toggle</button>
            <button class="delete-user" data-id="<%= user.id %>">Delete</button>
          </li>
        <% }); %>
      </ul>
    `),
    
    addOne: function(model) {
      // Add single model logic
    },
    
    addAll: function() {
      this.collection.each(this.addOne, this);
    },
    
    toggleUser: function(e) {
      const id = $(e.currentTarget).data('id');
      const model = this.collection.get(id);
      if (model) {
        model.toggle();
        model.save();
      }
    },
    
    deleteUser: function(e) {
      const id = $(e.currentTarget).data('id');
      const model = this.collection.get(id);
      if (model && confirm('Are you sure?')) {
        model.destroy();
      }
    }
  });
  
  // Return public API
  return {
    Model: MyModel,
    Collection: MyCollection,
    View: MyView,
    
    init: function(container) {
      const collection = new MyCollection();
      const view = new MyView({ 
        collection: collection,
        el: container
      });
      
      collection.fetch();
      return view;
    }
  };
});

// Anonymous AMD module
define(function() {
  return {
    utility: function(data) {
      return data.map(item => item.toUpperCase());
    }
  };
});

// AMD module with simplified wrapper
define(['exports'], function(exports) {
  exports.helper = function(value) {
    return value * 2;
  };
  
  exports.formatter = function(str) {
    return str.toLowerCase().replace(/\s+/g, '-');
  };
});