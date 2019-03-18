declare const require: any

import axios from "axios"
import Vue from 'vue'
import { Session } from '../common/common'

const templateString: string = require("html-loader!../views/home.html");

export const HomeVue = Vue.extend({
  name: "home",
  props: ['email', 'username'],
  data: function() {
    return {}
  },
  template: templateString,
  methods: {}
})
