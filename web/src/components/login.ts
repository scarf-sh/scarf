declare const require: any

const templateString: string = require("html-loader!../views/login.html");

export const LoginVue = {
  data() {
    return { hello: 'world' }
  },
  template: templateString
}
