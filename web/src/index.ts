import Vue from "vue";
import VueRouter from "vue-router"
import { LoginVue } from "./components/login";
import { SignUpVue } from "./components/signup";
import Buefy from 'buefy'
import 'buefy/dist/buefy.css'
import '../styles/defaults.scss'
import axios from 'axios'
import { HomeVue } from "./components/home";
import { Session } from './common/common'

Vue.use(Buefy)

const routes = [
  { path: '/login', component: LoginVue },
  { path: '/signup', component: SignUpVue },
  { name: 'home', path: '/home', component: HomeVue, props: true },
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
    session: { email: "", username: "" }
  },

  methods: {
    getSession: async function(): Promise<Session | null> {
      try {
        const response = await axios.get('http://localhost:9001/logged-in')
        return response.data
      } catch (e) {
        return null;
      }
    },
    handleErrorEvent: function(event: string) {
      this.errorMessage = event
      this.infoMessage = ""
    },
    handleInfoEvent: function(event: string) {
      this.infoMessage = event
      this.errorMessage = ""
    }
  },

  async beforeMount() {
    const session = await this.getSession()
    if (session) {
      this.session = <Session>session
      const currentQuery = this.$router.currentRoute.query
      // this is a very bad hack that i don't like. by the time we get here, if
      // we are already on the /home route, the props we pass in here are not
      // going to update. so by setting the route to home temporarily, when we
      // switch back to /home, props will be updated as expected. ugh.
      this.$router.push({ path: '/' })

      this.$router.push({ name: 'home', path: `/home`, params: { email: this.session.email, username: this.session.username } })
    } else {
      this.$router.push('/login')
    }
  }
}).$mount('#app');
