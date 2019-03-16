import Vue from "vue";
import VueRouter from "vue-router"
import { LoginVue } from "./components/login";
import { SignUpVue } from "./components/signup";
import * as JWT from 'jwt-decode'

import Buefy from 'buefy'
import 'buefy/dist/buefy.css'
import '../styles/defaults.scss'
import axios from 'axios'

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
    session: {}
  },

  methods: {
    getSession: async function(): Promise<any> {
      try {
        const response = await axios.get('http://localhost:9001/logged-in')
        return response.data
      } catch (e) {
        return null;
      }
    },
    handleErrorEvent: function(event: string) {
      this.errorMessage = event
    },
    handleInfoEvent: function(event: string) {
      this.infoMessage = event
    }
  },

  async created() {
    const session = await this.getSession()
    if (session) {
      this.$router.push('/home')
    } else {
      this.$router.push('/login')
    }
  }
}).$mount('#app');
