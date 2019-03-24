declare const require: any

import axios from "axios"
import Vue from 'vue'
import { SessionVue } from "../common/common";

const templateString: string = require("html-loader!../views/create-package.html");

export const CreatePackageVue = SessionVue.extend({
  name: "create-package",
  data: function() {
    return {
      createPackageRequest: { name: "", shortDescription: "", longDescription: "" }
    }
  },
  template: templateString,
  methods: {
    onNoSession: function() {
      this.$router.push({ path: '/login' })
    },
    submitCreatePackage: function() {
      axios.post("http://localhost:9001/package", this.createPackageRequest)
        .then(response => {
          console.log(response)
          this.$router.push({ path: '/home' })
        }).catch(err => {
          const message = err && err.response && err.response.data ?
            err.response.data : "Error creating your package"
          this.$emit("error-message", message)
        })
    }
  }
})
