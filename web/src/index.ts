import Vue from "vue";
import VueRouter from "vue-router"
import { LoginVue } from "./components/login";

const routes = [
  { path: '/login', component: LoginVue },
]

Vue.use(VueRouter);

const router = new VueRouter({
  routes
})

let v = new Vue({
  router,
  // el: "#app",
  data: {
    name: "World"
  },

  methods: {
    isLoggedIn: function() {
      return false
    }
  },

  created: function() {
    if (!this.isLoggedIn()) {
      router.push({ path: "login" })
    }
  }
}).$mount('#app');
