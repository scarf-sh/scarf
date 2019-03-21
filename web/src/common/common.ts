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
      this.$emit("error-message", message)
    },
    onNoSession: function(err: any) { },
    getSession: function(): Promise<Session | null> {
      return axios.get('http://localhost:9001/logged-in')
        .then(response => {
          this.$data.session = response.data
          return this.$data.session
        }).catch(err => {
          (<any>this).onNoSession(err)
        })
    },
  },

  beforeMount: function() {
    const self = <any>this
    self.clearMessages()
    return self.getSession()
  }
})
