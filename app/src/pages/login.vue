<script>
import { useTheme } from 'vuetify'
import logo from '@images/logo.svg?raw'
import authV1MaskDark from '@images/pages/auth-v1-mask-dark.png'
import authV1MaskLight from '@images/pages/auth-v1-mask-light.png'
import authV1Tree2 from '@images/pages/auth-v1-tree-2.png'
import authV1Tree from '@images/pages/auth-v1-tree.png'
import { ref, computed } from 'vue'

export default {
  setup() {
    const form = ref({
      cpf: '',
      password: '',
      remember: false,
    })
    const vuetifyTheme = useTheme()

    const authThemeMask = computed(() => {
      return vuetifyTheme.global.name.value !== 'light' ? authV1MaskLight : authV1MaskDark
    })

    const isPasswordVisible = ref(false)

    return {
      logo,
      authV1Tree2,
      authV1Tree,
      form,
      authThemeMask,
      isPasswordVisible,
    }
  },
  mounted() {
    this.logOut()
  },
  methods: {
    submitForm() {
      const {cpf, password} = this.form
      this.$http.post('/login', {
        cpf,
        senha: password,
      })
      .then(({data}) => this.authenticateLogin(data))
      .catch(x => console.error('Erro ao realizar o login', x))
    },
    authenticateLogin(data){
      if(data.result === 'success') {
          this.logIn(data.id)
        }
        else {
          this.logOut()
          alert(data.error.message)
        }
    },
    logIn(id){
      localStorage.setItem('pp-correntista-id', id)
      this.$router.push('dashboard')
    },
    logOut() {
      localStorage.removeItem('pp-correntista-id')
    }
  }
}
</script>


<template>
  <div class="auth-wrapper d-flex align-center justify-center pa-4">
    <VCard
      class="auth-card pa-4 pt-7"
      max-width="448"
    >
      <VCardItem class="justify-center">
        <template #prepend>
          <div class="d-flex">
            <div v-html="logo" />
          </div>
        </template>

        <VCardTitle class="font-weight-semibold text-2xl text-uppercase">
          Materio
        </VCardTitle>
      </VCardItem>

      <VCardText class="pt-2">
        <h5 class="text-h5 font-weight-semibold mb-1">
          Welcome to Materio! 👋🏻
        </h5>
        <p class="mb-0">
          Please sign-in to your account and start the adventure
        </p>
      </VCardText>

      <VCardText>
        <VForm @submit.prevent="submitForm">
          <VRow>
            <!-- cpf -->
            <VCol cols="12">
              <VTextField
                v-mask="'###.###.###-##'"
                v-model="form.cpf"
                label="CPF"
                type="text"
              />
            </VCol>

            <!-- password -->
            <VCol cols="12">
              <VTextField
                v-model="form.password"
                label="Password"
                :type="isPasswordVisible ? 'text' : 'password'"
                :append-inner-icon="isPasswordVisible ? 'mdi-eye-off-outline' : 'mdi-eye-outline'"
                @click:append-inner="isPasswordVisible = !isPasswordVisible"
              />

              <!-- remember me checkbox -->
              <div class="d-flex align-center justify-space-between flex-wrap mt-1 mb-4"></div>

              <!-- login button -->
              <VBtn
                block
                type="submit"
              >
                Login
              </VBtn>
            </VCol>
          </VRow>
        </VForm>
      </VCardText>
    </VCard>
  </div>
</template>

<style lang="scss">
@use "@core/scss/pages/page-auth.scss";
</style>
