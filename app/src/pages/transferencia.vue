<script setup>
import router from '@/router';
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import FormTransferencia from '@/views/pages/form-layouts/FormTransferencia.vue';
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
        <VCard title="Transferência">
          <VCardText>
            <FormTransferencia />
          </VCardText>
        </VCard>
      </VCol>
    </VRow>
  </div>
</template>
