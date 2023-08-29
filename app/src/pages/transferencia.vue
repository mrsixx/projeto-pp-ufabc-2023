<script setup>
import router from '@/router';
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import FormTransferencia from '@/views/pages/form-layouts/FormTransferencia.vue';
import { onMounted } from 'vue';
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
        <VCard title="Transferência">
          <VCardText>
            <FormTransferencia @show-error="showErrorHandler" />
          </VCardText>
        </VCard>
      </VCol>
    </VRow>
  </div>
</template>
