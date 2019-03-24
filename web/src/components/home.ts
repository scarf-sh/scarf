declare const require: any

import axios from "axios"
import Vue from 'vue'
import { Session, SessionVue } from '../common/common'

const templateString: string = require("html-loader!../views/home.html");

export const HomeVue = SessionVue.extend({
  name: "home",
  data: function() {
    return { packages: [], isLoading: false }
  },
  template: templateString,
  methods: {
    onNoSession: function() {
      this.$router.push({ path: '/login' })
    },
  },
  created: async function() {
    await axios.get('http://localhost:9001/packages')
      .then(response => {
        this.packages = response.data.packages
      }).catch(err => {
        this.emitError((err.data || {}).message || "Error getting your package list")
      })
  }
})
