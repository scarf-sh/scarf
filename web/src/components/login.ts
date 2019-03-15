declare const require: any

import axios from "axios"
import Vue from 'vue'

const templateString: string = require("html-loader!../views/login.html");

export const LoginVue = Vue.extend({
  name: "login",
  data: function() {
    return {
      loginRequest: { email: null, password: null }
    }
  },
  template: templateString,
  methods: {
    submitLogin: function() {
      axios.post("http://localhost:9001/login", this.loginRequest)
        .then(response => {
          console.log(response)
        })
    }
  }
})
