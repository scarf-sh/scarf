declare const require: any

import axios from "axios"
import Vue from 'vue'

const templateString: string = require("html-loader!../views/signup.html");

export const SignUpVue = Vue.extend({
  data: function() {
    return { signUpRequest: { email: null, username: null, password: null }, passwordConfirm: null }
  },
  template: templateString,

  methods: {
    submitSignUp: function() {
      axios.post("http://localhost:9001/user", this.signUpRequest)
        .then(response => {
          console.log(response)
          window.location.reload()
        })
    },

    beforeMount: function() {
      this.$emit("info-message", { message: "" })
    }

  }
})
