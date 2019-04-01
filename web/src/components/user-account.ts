declare const require: any

import axios from "axios"
import Vue from 'vue'
import { Session, SessionVue } from '../common/common'

const templateString: string = require("html-loader!../views/user-account.html");

export const UserAccountVue = SessionVue.extend({
  name: "user-account",
  data: function() {
    return {
      packages: [],
      isLoading: false,
      apiToken: "Loading...",
      currentPassword: "",
      newPassword: "",
      confirmNewPassword: ""
    }
  },
  template: templateString,
  methods: {
    onNoSession: function() {
      this.$router.push({ path: '/login' })
    },
    regenerateApiToken: async function() {
      this.isLoading = true
      await axios.post('http://localhost:9001/user/at')
        .then(response => {
          this.apiToken = response.data.apiToken
          this.isLoading = false
        }).catch(err => {
          this.emitError((err.data || {}).message || "Error resetting your api token")
          this.isLoading = false
        })
    }
  },
  created: async function() {
    this.isLoading = true
    await axios.get('http://localhost:9001/user/account')
      .then(response => {
        this.apiToken = response.data.apiToken
        this.isLoading = false
      }).catch(err => {
        this.emitError((err.data || {}).message || "Error getting your api token")
        this.isLoading = false
      })
  }
})
