declare const require: any

import axios from "axios"
import Vue from 'vue'
import { Session, SessionVue } from '../common/common'

const templateString: string = require("html-loader!../views/package-details.html");

export const PackageDetailsVue = SessionVue.extend({
  name: "packageDetails",
  data: function() {
    return {
      package: {},
      releases: [],
      isLoading: false,
      packageName: this.$router.currentRoute.params.packageName,
      stats: [],
      totalInstalls: 0
    }
  },
  template: templateString,
  methods: {
  },
  created: async function() {
    await axios.get(`http://localhost:9001/package/${this.packageName}`)
      .then(async response => {
        this.package = response.data.package
        this.releases = response.data.releases
        this.totalInstalls = response.data.totalInstalls
        if (this.session.username) {
          const statsResponse = await axios.get(`http://localhost:9001/package/stats/${this.packageName}`)
          this.stats = statsResponse.data.stats
        }
        return Promise.resolve()
      }).catch(err => {
        this.emitError((err.data || {}).message || "Error getting package details")
      })
  }
})
