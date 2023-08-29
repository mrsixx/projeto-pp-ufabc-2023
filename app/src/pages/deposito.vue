<script setup>
import router from '@/router';
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import FormDeposito from '@/views/pages/form-layouts/FormDeposito.vue';
const correntistaStore = useCorrentistaStore()
const showError = ref(false)
const errorMessage = ref('')
function showErrorHandler(error) {
  showError.value = true
  errorMessage.value = `${error.code}: ${error.message}`
}
onMounted(() => {
  if(!correntistaStore.loaded)
    router.replace('/login')
})
</script>

<template>
  <div>
    <VSnackbar v-model="showError">
      {{ errorMessage }}
    </VSnackbar>
    <VRow>
      <VCol
        cols="12"
        md="6"
        offset="3"
      >
        <!-- 👉 Vertical Form -->
        <VCard title="Depósito">
          <VCardText>
            <FormDeposito  @show-error="showErrorHandler"/>
          </VCardText>
        </VCard>
      </VCol>
    </VRow>
  </div>
</template>
