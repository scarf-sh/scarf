declare const require: any

import axios from "axios"
import Vue from 'vue'
import { Session, SessionVue } from '../common/common'

const templateString: string = require("html-loader!../views/package-search.html");

export const PackageSearchVue = SessionVue.extend({
  name: "package-search",
  data: function() {
    return {
      results: [],
      isLoading: false,
      query: this.$router.currentRoute.params.query
    }
  },
  template: templateString,
  methods: {
  },
  created: function() {
    return axios.get(`http://localhost:9001/packages/search/${this.$data.query}`)
      .then(response => {
        this.$data.results = response.data.results
        return Promise.resolve()
      }).catch(err => {
        this.emitError((err.data || {}).message || "Error getting search results")
      })
  }
})
