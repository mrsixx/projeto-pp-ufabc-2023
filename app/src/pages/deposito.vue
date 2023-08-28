<script setup>
import router from '@/router';
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import FormDeposito from '@/views/pages/form-layouts/FormDeposito.vue';
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
        <VCard title="Depósito">
          <VCardText>
            <FormDeposito />
          </VCardText>
        </VCard>
      </VCol>
    </VRow>
  </div>
</template>
