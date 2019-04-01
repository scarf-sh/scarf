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
import { CreatePackageVue } from "./components/create-package";
import { PackageDetailsVue } from "./components/package-details";
import { UserAccountVue } from "./components/user-account";

Vue.use(Buefy)

const routes = [
  { path: '/login', component: LoginVue },
  { path: '/signup', component: SignUpVue },
  { path: '/create-package', component: CreatePackageVue, props: true },
  { path: '/package/:username/:packageName', component: PackageDetailsVue },
  { path: '/home', component: HomeVue, props: true },
  { path: '/user-account', component: UserAccountVue, props: true },
  { path: '/', component: HomeVue },
]

Vue.use(VueRouter);

const router = new VueRouter({
  routes
})

let v = new Vue({
  router,
  data: function() {
    return {
      infoMessage: "",
      errorMessage: "",
      session: { email: "", username: "" }
    }
  },

  methods: {
    handleErrorEvent: function(event: string) {
      this.errorMessage = event
      this.infoMessage = ""
    },
    handleInfoEvent: function(event: string) {
      this.infoMessage = event
      this.errorMessage = ""
    },
    logOut: function() {
      // removing the xsrf token invalidates our session
      document.cookie = "XSRF-TOKEN="
      this.$router.push({ path: "/login" })
    }
  },

  beforeMount() {
    const path = this.$router.currentRoute.path || "/"
    if (path === "/" || !path) {
      this.$router.push({ path: "/home" })
    }
  }

}).$mount('#app');
