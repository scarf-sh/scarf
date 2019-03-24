declare const require: any

import axios from "axios"
import Vue from 'vue'
import { Session, SessionVue } from '../common/common'

const templateString: string = require("html-loader!../views/package-details.html");

export const PackageDetailsVue = SessionVue.extend({
  name: "packageDetails",
  data: function() {
    return { package: {}, releases: [], isLoading: false, packageName: this.$router.currentRoute.params.packageName }
  },
  template: templateString,
  methods: {
  },
  created: async function() {
    await axios.get(`http://localhost:9001/package/${this.packageName}`)
      .then(response => {
        this.package = response.data.package
        this.releases = response.data.releases
      }).catch(err => {
        this.emitError((err.data || {}).message || "Error getting your package list")
      })
  }
})
