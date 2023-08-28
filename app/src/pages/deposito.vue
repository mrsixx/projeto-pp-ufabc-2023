<script setup>
import FormDeposito from '@/views/pages/form-layouts/FormDeposito.vue';
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
