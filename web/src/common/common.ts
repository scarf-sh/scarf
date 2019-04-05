import Vue from 'vue'
import axios from 'axios'

export interface Session {
  email: string
  username: string
}

export const SessionVue = Vue.extend({

  data: function() {
    return {
      session: { email: "", username: "" }
    }
  },

  methods: {
    clearMessages: function() {
      this.$emit("error-message", "")
      this.$emit("info-message", "")
    },
    emitError: function(message: string) {
      this.clearMessages()
      this.$emit("error-message", message)
    },
    emitInfo: function(message: string) {
      this.clearMessages()
      this.$emit("info-message", message)
    },
    onNoSession: function(err?: any) { },
    onValidSession: function(session: Session) { },
    getSession: function() {
      return axios.get('http://localhost:9001/logged-in')
        .then(response => {
          // TODO (|#frontend) - figure out a way to share the session globally
          this.$root.$data.session = response.data
          this.session = response.data
          return this.onValidSession(this.$data.session)
        }).catch(err => {
          this.$root.$data.session = { email: "", username: "" }
          this.session = { email: "", username: "" }
          return (<any>this).onNoSession(err)
        })
    },
    isLoggedIn: function() {
      return !!this.session.username
    }
  },

  beforeMount: function() {
    const self = <any>this
    self.clearMessages()
    return self.getSession()
  }
})
