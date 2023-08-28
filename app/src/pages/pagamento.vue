<script setup>
import router from '@/router';
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import FormPagamento from '@/views/pages/form-layouts/FormPagamento.vue';
import { onMounted } from 'vue';
const correntistaStore = useCorrentistaStore()
onMounted(() => {
  if(!correntistaStore.loaded)
    router.replace('/login')
})
</script>

<template>
  <div>
    <VRow>
      <VCol
        cols="12"
        md="6"
        offset="3"
      >
        <!-- 👉 Vertical Form -->
        <VCard title="Pagamento">
          <VCardText>
            <FormPagamento />
          </VCardText>
        </VCard>
      </VCol>
    </VRow>
  </div>
</template>
