declare const require: any

import axios from "axios"
import Vue from 'vue'

const templateString: string = require("html-loader!../views/create-package.html");

export const CreatePackageVue = Vue.extend({
  name: "create-package",
  data: function() {
    return {
      packageFile: null,
      createPackageRequest: { email: null, password: null }
    }
  },
  template: templateString,
  methods: {
    submitCreatePackage: function() {
      axios.post("http://localhost:9001/createPackage", this.createPackageRequest)
        .then(response => {
          console.log(response)
          window.location.reload()
        }).catch(err => {
          const message = err && err.response && err.response.data ?
            err.response.data : "Error logging you in"
          this.$emit("error-message", message)
        })
    }
  }
})
