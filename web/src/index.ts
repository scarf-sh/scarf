import Vue from "vue";
import VueRouter from "vue-router"
import { LoginVue } from "./components/login";
import { SignUpVue } from "./components/signup";
import * as JWT from 'jwt-decode'

import Buefy from 'buefy'
import 'buefy/dist/buefy.css'
import '../styles/defaults.scss'

Vue.use(Buefy)

const routes = [
  { path: '/login', component: LoginVue },
  { path: '/signup', component: SignUpVue },
]

Vue.use(VueRouter);

const router = new VueRouter({
  routes
})

let v = new Vue({
  router,
  data: {
    infoMessage: "",
    errorMessage: "",
  },

  methods: {
    isLoggedIn: function(): boolean {
      console.log(document.cookie)
      return false
    },
    handleErrorEvent: function(event: string) {
      this.errorMessage = event
    },
    handleInfoEvent: function(event: string) {
      this.infoMessage = event
    }
  },

  created: function() {
    if (!this.isLoggedIn()) {
      router.push({ path: "login" })
    }
  }
}).$mount('#app');
